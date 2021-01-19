pub mod parser;
pub mod ast;
pub mod ir;
pub mod ir2ass;

pub fn run(_input: &str, _output: &mut impl std::io::Write) -> std::io::Result<()> {
  let ast = parser::Parser {}.parse(&mut parser::Lexer::new(_input.as_bytes())).expect("failed to parse input");
  let p = ir::ast2ir(&ast);
  ir2ass::ir2assembly(&p, _output)
}