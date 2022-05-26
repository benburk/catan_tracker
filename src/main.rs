use card_tracker::Tracker;
use serde_json::{json, Value};

mod card_tracker;
mod util;

use crate::util::parse_html;
use util::N_PLAYERS;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut tracker = Tracker::new();

    // register players
    let args: Vec<String> = std::env::args().collect();
    if args.len() > 1 {
        if args.len() != N_PLAYERS + 1 {
            println!("Expected {} players", N_PLAYERS);
            std::process::exit(1);
        }
        for name in &args[1..] {
            tracker.get_player_index(name);
        }
    }

    let mut browser = cdp_client::Browser::new("http://localhost:9222/json")?;
    loop {
        let response = browser.send(
            "Runtime.evaluate",
            json!({
                "expression": r#"document.getElementById("game-log-text").outerHTML"#
            }),
        )?;
        if let Value::String(html) = &response["result"]["value"] {
            let lines = parse_html(html);
            tracker.parse_log(&lines);
        } else {
            println!("Failed to find game log.");
        }
        println!("{}", tracker.build_table());
        let input = util::input("> ");
        tracker.parse_command(&input);
    }
}
