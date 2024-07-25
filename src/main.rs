use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::sync::{Arc, Mutex};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
use tree_sitter::{self, Node, Parser, Query, QueryCursor};

const NAME: &str = env!("CARGO_PKG_NAME");
const VERSION: &str = env!("CARGO_PKG_VERSION");

const GRINGO_SHARP_KEYWORDS: [&str; 26] = [
    "#const",
    "#count",
    "#disjoint",
    "#external",
    "#defined",
    "#false",
    "#include",
    "#inf",
    "#infimum",
    "#max",
    "#maximize",
    "#maximise",
    "#min",
    "#minimize",
    "#minimise",
    "#show",
    "#edge",
    "#project",
    "#heuristic",
    "#sum",
    "#sum+",
    "#sup",
    "#supremum",
    "#true",
    "#program",
    "#theory",
];

#[derive(Debug)]
struct GringoLanguageServerBackend {
    client: Client,
    documents: Arc<Mutex<HashMap<Url, DocumentState>>>,
}

#[derive(Debug)]
struct DocumentState {
    contents: String,
    predicates: HashSet<Predicate>,
    constants: HashSet<String>,
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
struct Predicate {
    name: String,
    arity: usize,
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
struct ConstantSymbol {
    name: String,
}

#[derive(Debug, Clone)]
struct VariableSymbol<'a> {
    name: String,
    node: Node<'a>,
}

impl<'a> PartialEq for VariableSymbol<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl<'a> Eq for VariableSymbol<'a> {}

impl<'a> Hash for VariableSymbol<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

#[derive(Debug, Clone)]
struct PredicateSymbol<'a> {
    name: String,
    arity: usize,
    node: Node<'a>,
}

impl<'a> PartialEq for PredicateSymbol<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.arity == other.arity
    }
}

impl<'a> Eq for PredicateSymbol<'a> {}

impl<'a> Hash for PredicateSymbol<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.arity.hash(state);
    }
}

impl GringoLanguageServerBackend {
    async fn process_document(&self, document_url: Url, contents: &str) {
        let contents_bytes = contents.as_bytes();
        let mut diagnostics = Vec::new();

        let mut parser = Parser::new();
        parser
            .set_language(&tree_sitter_gringo::language())
            .expect("Error loading clingo grammar");
        let tree = parser.parse(contents, None).expect("Error parsing code");
        let root_node = tree.root_node();

        let statement_query = Query::new(&tree_sitter_gringo::language(), "(statement) @statement")
            .expect("Error compiling query");
        let head_body_query = Query::new(
            &tree_sitter_gringo::language(),
            "(head) @head (bodydot) @bodydot",
        )
        .expect("Error compiling query");

        let mut statement_cursor = QueryCursor::new();
        let statement_matches =
            statement_cursor.matches(&statement_query, root_node, contents_bytes);

        let mut predicates: Vec<HashSet<PredicateSymbol>> = vec![];
        predicates.push(HashSet::new());
        predicates.push(HashSet::new());
        let mut constants: HashSet<ConstantSymbol> = HashSet::new();

        for statement_node in statement_matches
            .map(|m| m.captures)
            .flatten()
            .map(|m| m.node)
        {
            let mut cursor = QueryCursor::new();
            let matches = cursor.matches(&head_body_query, statement_node, contents_bytes);

            let mut statement_variables: Vec<HashSet<VariableSymbol>> = vec![];
            statement_variables.push(HashSet::new());
            statement_variables.push(HashSet::new());
            let mut statement_predicates: Vec<HashSet<PredicateSymbol>> = vec![];
            statement_predicates.push(HashSet::new());
            statement_predicates.push(HashSet::new());
            let mut statement_constants: HashSet<ConstantSymbol> = HashSet::new();

            let mut has_head = false;

            for m in matches {
                if m.pattern_index == 0 {
                    has_head = true;
                }
                for capture in m.captures {
                    let node = capture.node;

                    if m.pattern_index == 0 {
                        let head_aggregate_query = Query::new(
                            &tree_sitter_gringo::language(),
                            "(headaggregate) @headggregate (disjunction) @disjunction",
                        )
                        .expect("Error compiling query");
                        let mut cursor = QueryCursor::new();
                        let head_aggregate_matches =
                            cursor.matches(&head_aggregate_query, node, contents_bytes);

                        for head_aggregate_node in head_aggregate_matches
                            .map(|m| m.captures)
                            .flatten()
                            .map(|m| m.node)
                        {
                            let optcondition_query = Query::new(
                                &tree_sitter_gringo::language(),
                                "(optcondition) @optcondition (disjunction (COLON) (litvec) @litvec)",
                            )
                            .expect("Error compiling query");
                            let mut cursor = QueryCursor::new();
                            let optcondition_matches = cursor.matches(
                                &optcondition_query,
                                head_aggregate_node,
                                contents_bytes,
                            );
                            for optcondition_node in optcondition_matches
                                .map(|m| m.captures)
                                .flatten()
                                .map(|m| m.node)
                            {
                                let variable_query = Query::new(
                                    &tree_sitter_gringo::language(),
                                    "(VARIABLE) @variable",
                                )
                                .expect("Error compiling query");
                                let mut cursor = QueryCursor::new();
                                let variable_matches = cursor.matches(
                                    &variable_query,
                                    optcondition_node,
                                    contents_bytes,
                                );
                                for variable_node in variable_matches
                                    .map(|m| m.captures)
                                    .flatten()
                                    .map(|m| m.node)
                                {
                                    let variable_text =
                                        variable_node.utf8_text(contents_bytes).unwrap();
                                    statement_variables[1].insert(VariableSymbol {
                                        name: String::from(variable_text),
                                        node: variable_node,
                                    });
                                }
                            }
                        }
                    }

                    let mut cursor = QueryCursor::new();
                    let variable_query =
                        Query::new(&tree_sitter_gringo::language(), "(VARIABLE) @variable")
                            .expect("Error compiling query");
                    let variable_matches = cursor.matches(&variable_query, node, contents_bytes);
                    for variable_node in variable_matches
                        .map(|m| m.captures)
                        .flatten()
                        .map(|m| m.node)
                    {
                        let variable_text = variable_node.utf8_text(contents_bytes).unwrap();
                        statement_variables[m.pattern_index].insert(VariableSymbol {
                            name: String::from(variable_text),
                            node: variable_node,
                        });
                    }

                    let mut cursor = QueryCursor::new();
                    let atom_query =
                        Query::new(&tree_sitter_gringo::language(), "(atom (identifier)) @atom")
                            .expect("Error compiling query");
                    let atom_matches = cursor.matches(&atom_query, node, contents_bytes);
                    for atom_node in atom_matches.map(|m| m.captures).flatten().map(|m| m.node) {
                        let predicate_node = atom_node
                            .children(&mut atom_node.walk())
                            .filter(|x| x.kind() == "identifier")
                            .next();
                        if predicate_node.is_none() {
                            continue;
                        }
                        let predicate_node = predicate_node.unwrap();
                        let name = predicate_node.utf8_text(contents_bytes).unwrap();

                        let argvec_node = atom_node
                            .children(&mut atom_node.walk())
                            .filter(|x| x.kind() == "argvec")
                            .next();
                        let mut arities: HashSet<usize> = HashSet::new();
                        if let Some(argvec_node) = argvec_node {
                            let mut argvec_walk = argvec_node.walk();
                            let termvec_nodes = argvec_node
                                .children(&mut argvec_walk)
                                .filter(|x| x.kind() == "termvec");
                            for termvec_node in termvec_nodes {
                                let mut termvec_walk = termvec_node.walk();
                                let term_nodes = termvec_node
                                    .children(&mut termvec_walk)
                                    .filter(|x| x.kind() == "term");
                                arities.insert(term_nodes.count());
                            }
                        } else {
                            arities.insert(0);
                        }
                        for arity in arities {
                            statement_predicates[m.pattern_index].insert(PredicateSymbol {
                                name: String::from(name),
                                arity,
                                node,
                            });
                        }
                    }

                    let term_name_query = Query::new(
                        &tree_sitter_gringo::language(),
                        "(term (identifier) @identifier)",
                    )
                    .expect("Error compiling query");
                    let mut cursor = QueryCursor::new();
                    let matches = cursor.matches(&term_name_query, node, contents_bytes);
                    for constant_node in matches.map(|m| m.captures).flatten().map(|m| m.node) {
                        let constant_name = constant_node.utf8_text(contents_bytes).unwrap();
                        statement_constants.insert(ConstantSymbol {
                            name: String::from(constant_name),
                        });
                    }

                    constants.extend(statement_constants.iter().cloned());
                    predicates[m.pattern_index]
                        .extend(statement_predicates[m.pattern_index].iter().cloned());
                }
            }

            if has_head {
                let vars_not_in_body = statement_variables[0].difference(&statement_variables[1]);
                for var in vars_not_in_body {
                    let mut diagnostic = Diagnostic::default();
                    let range = var.node.range();
                    diagnostic.range = Range {
                        start: Position {
                            line: range.start_point.row as u32,
                            character: range.start_point.column as u32,
                        },
                        end: Position {
                            line: range.end_point.row as u32,
                            character: range.end_point.column as u32,
                        },
                    };
                    diagnostic.message = format!("'{}' is unsafe", var.name);
                    diagnostics.push(diagnostic);
                }
            }
        }

        let preds_not_in_head = predicates[1].difference(&predicates[0]);
        for pred in preds_not_in_head {
            let mut diagnostic = Diagnostic::default();
            let range = pred.node.range();
            diagnostic.range = Range {
                start: Position {
                    line: range.start_point.row as u32,
                    character: range.start_point.column as u32,
                },
                end: Position {
                    line: range.end_point.row as u32,
                    character: range.end_point.column as u32,
                },
            };
            diagnostic.message = format!(
                "'{}/{}' does not appear in any rule head",
                pred.name, pred.arity
            );
            diagnostic.severity = Some(DiagnosticSeverity::WARNING);
            diagnostics.push(diagnostic);
        }

        let document_state = DocumentState {
            contents: contents.to_string(),
            predicates: predicates[0]
                .iter()
                .map(|x| Predicate {
                    name: x.name.clone(),
                    arity: x.arity,
                })
                .collect(),
            constants: constants.iter().map(|x| x.name.clone()).collect(),
        };
        self.documents
            .lock()
            .unwrap()
            .insert(document_url.clone(), document_state);

        self.client
            .publish_diagnostics(document_url, diagnostics, Some(1))
            .await;
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for GringoLanguageServerBackend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        let server_info = Some(ServerInfo {
            name: String::from(NAME),
            version: Some(String::from(VERSION)),
        });

        let mut capabilities = ServerCapabilities::default();
        capabilities.text_document_sync =
            Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL));
        capabilities.completion_provider = Some(CompletionOptions::default());

        Ok(InitializeResult {
            capabilities,
            server_info,
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::ERROR, "Server initialized.")
            .await;
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.process_document(params.text_document.uri, params.text_document.text.as_str())
            .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.process_document(
            params.text_document.uri,
            params.content_changes.first().unwrap().text.as_str(),
        )
        .await;
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let position = params.text_document_position.position;
        let document_uri = params.text_document_position.text_document.uri;
        let documents = self.documents.lock().unwrap();
        if !documents.contains_key(&document_uri) {
            return Ok(None);
        }
        let document_state = documents.get(&document_uri).unwrap();

        let mut completions: Vec<CompletionItem> = vec![];
        for predicate in &document_state.predicates {
            let Predicate { name, arity } = predicate;
            let mut completion = CompletionItem::default();
            completion.label = format!("{}/{}", name, arity);
            completion.kind = Some(CompletionItemKind::FUNCTION);
            let mut insert_text = format!("{}", name);
            if *arity != 0usize {
                insert_text += "(";
                if *arity > 0usize {
                    insert_text += &"_, ".repeat(*arity - 1);
                }
                insert_text += "_)";
            }
            completion.insert_text = Some(insert_text);
            completions.push(completion);
        }
        for name in &document_state.constants {
            let mut completion = CompletionItem::default();
            completion.label = name.clone();
            completion.kind = Some(CompletionItemKind::CONSTANT);
            completions.push(completion);
        }

        let contents = document_state.contents.clone();
        let line = contents
            .lines()
            .nth(position.line as usize)
            .unwrap_or_default();
        let mut start = position.character as usize - 1;
        while start > 0
            && line.is_char_boundary(start)
            && (line[start..=start]
                .chars()
                .next()
                .unwrap_or_default()
                .is_alphanumeric()
                || &line[start..=start] == "_"
                || &line[start..=start] == "'")
        {
            start -= 1;
        }

        if &line[start..=start] == "#" {
            for keyword in GRINGO_SHARP_KEYWORDS {
                let mut completion = CompletionItem::default();
                completion.label = String::from(keyword);
                completion.kind = Some(CompletionItemKind::KEYWORD);
                completions.push(completion);
            }
        }

        Ok(Some(CompletionResponse::Array(completions)))
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();
    let (service, socket) = LspService::new(|client| GringoLanguageServerBackend {
        client,
        documents: Arc::new(Mutex::new(HashMap::new())),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
