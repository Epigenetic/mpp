use std::io;
mod lexer;

fn main() {
    loop {
        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(_n) => {
                if input.eq("h\r\n") {
                    break;
                }
                let mut tokenizer = lexer::Tokenizer::new(input);
                let tokens = tokenizer.tokenize();
                for token in tokens {
                    println!("{}", token);
                }
            }
            Err(err) => println!("{}", err),
        }
    }
}
