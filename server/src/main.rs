use std::io::Read;

fn main() {
    parse_input();
}

fn parse_input() {
    let mut input = String::new();
    std::io::stdin().read_to_string(&mut input).expect("stdin");
    let parse = parser::parse(&input);
    println!("{}\n", parse.debug_tree());
}
