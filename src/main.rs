use regex::Regex;
use scraper::{Html, Selector};
use serde_json::{json, Value};
use std::collections::HashMap;
mod util;

const N_PLAYERS: usize = 4;
const N_RESOURCES: usize = 5;
const NAME: &str = r"(\w+(?:#\d+)?)";
const CARDS: &str = r"((?:(?:lumber|brick|wool|grain|ore|card) ?)+)";
const ITEM_PTTN: &str = r"(road|settlement|city|development card)";

const USERNAME: &str = "Mera#4025";

type Hand = [u32; N_RESOURCES];
type State = [Hand; N_PLAYERS];

/// Table format string with 7 columns
macro_rules! format_str {
    () => {
        "{:<10} | {:<10} | {:<10} | {:<10} | {:<10} | {:<10} | {:<10}\n"
    };
}

fn normalize_text(text: &str) -> String {
    let text = Regex::new(r"\s+")
        .unwrap()
        .replace_all(text, " ")
        .to_string();

    let text = Regex::new(r"\s+$")
        .unwrap()
        .replace_all(&text, "")
        .to_string();

    let text = Regex::new(r"\b(?i)you\b")
        .unwrap()
        .replace_all(&text, USERNAME)
        .to_string();

    text
}

/// Parses the html into log messages
fn parse_html(html: &str) -> Vec<String> {
    let mut lines = Vec::new();

    let document = Html::parse_document(html);
    let msg_selector = Selector::parse(".message_post").unwrap();
    let img_selector = Selector::parse("img").unwrap();

    for message in document.select(&msg_selector) {
        let mut text = message.inner_html();
        for img in message.select(&img_selector) {
            let alt_text = format!("{} ", img.value().attr("alt").unwrap());
            text = text.replace(&img.html(), &alt_text);
        }
        if text.contains("<hr") {
            continue;
        }
        lines.push(text);
    }
    lines
}

/// Converts an iterator of counts to a Hand
fn new_cards_from(src: impl Iterator<Item = u32>) -> Hand {
    let mut result = Hand::default();
    for (i, c) in src.enumerate() {
        result[i] = c;
    }
    result
}

/// Parses cards with any amount of whitespace between them.
fn to_cards(cards: &str) -> Hand {
    let re = Regex::new(r"(lumber|brick|wool|grain|ore)").unwrap();

    let mut result = Hand::default();
    for capture in re.captures_iter(cards) {
        match capture.get(0).unwrap().as_str() {
            "lumber" => result[0] += 1,
            "brick" => result[1] += 1,
            "wool" => result[2] += 1,
            "grain" => result[3] += 1,
            "ore" => result[4] += 1,
            _ => unreachable!(),
        }
    }
    result
}

/// Builds all the regex patterns we can encounter in our log
fn build_patterns() -> [(Regex, fn(&mut Tracker, &[&str]) -> ()); 9] {
    [
        (
            Regex::new(&format!(
                r"{NAME} (?:got|received starting resources): *{CARDS}"
            ))
            .unwrap(),
            Tracker::add_event,
        ),
        (
            Regex::new(&format!(r"{NAME} discarded: *{CARDS}")).unwrap(),
            Tracker::discard_event,
        ),
        (
            Regex::new(&format!(r"{NAME} (?:built a|bought) {ITEM_PTTN}")).unwrap(),
            Tracker::spend_event,
        ),
        (
            Regex::new(&format!(r"{NAME} stole:? {CARDS} from:? {NAME}")).unwrap(),
            Tracker::rob_event,
        ),
        (
            Regex::new(&format!(r"{NAME} wants to give: {CARDS} for: {CARDS}")).unwrap(),
            Tracker::handle_trade_offer,
        ),
        (
            Regex::new(&format!(
                r"{NAME} traded: {CARDS} for: {CARDS} with: {NAME}"
            ))
            .unwrap(),
            Tracker::handle_trade,
        ),
        (
            Regex::new(&format!(r"{NAME} took from bank: {CARDS}")).unwrap(),
            Tracker::handle_year_of_plenty,
        ),
        (
            Regex::new(&format!(r"{NAME} gave bank: {CARDS} and took {CARDS}")).unwrap(),
            Tracker::handle_bank_trade,
        ),
        (
            Regex::new(&format!(r"{NAME} stole \d+: {CARDS}")).unwrap(),
            Tracker::monopoly_event,
        ),
    ]
}

struct Tracker {
    player_idx: HashMap<String, usize>,
    players: [usize; N_PLAYERS],
    states: HashMap<State, u32>,
    events: [(Regex, fn(&mut Self, &[&str]) -> ()); 9],
    last_line: usize,
}

impl Tracker {
    pub fn new() -> Self {
        Self {
            player_idx: HashMap::new(),
            players: [0, 1, 2, 3],
            states: HashMap::from([(State::default(), 1)]),
            events: build_patterns(),
            last_line: 0,
        }
    }

    /// Parse new log
    pub fn parse_log<T: AsRef<str>>(&mut self, messages: &[T]) {
        for (i, line) in messages.iter().enumerate() {
            if i <= self.last_line {
                continue;
            }
            let line = normalize_text(line.as_ref());
            for (regex, event) in &self.events {
                if let Some(caps) = regex.captures(line.as_ref()) {
                    let args = caps
                        .iter()
                        .skip(1)
                        .map(|m| m.unwrap().as_str())
                        .collect::<Vec<_>>();
                    event(self, &args);
                    self.last_line = i;
                    break; // there should only be one event to parse per line
                }
            }
        }
    }

    /// Gets the player index for the given name
    /// If the player is not in the tracker, it will be added
    fn get_player_index(&mut self, name: &str) -> usize {
        let i = self.player_idx.len();
        let tmp = *self.player_idx.entry(name.to_owned()).or_insert(i);
        if self.player_idx.len() > N_PLAYERS {
            panic!(
                "Too many players! {:?}",
                self.player_idx.keys().collect::<Vec<_>>()
            );
        }
        return tmp;
    }

    /// Removes states where player does not have that many cards.
    fn know_has(&mut self, name: &str, cards: &Hand) {
        let i = self.get_player_index(name);

        self.states
            .retain(|state, _| state[i].iter().zip(cards.iter()).all(|(a, b)| a >= b));

        assert!(self.states.len() > 0);
    }

    /// Adds a `Hand` of cards to every state for a player
    fn add_cards(&mut self, name: &str, cards: &Hand) {
        let i = self.get_player_index(name);
        self.states = self
            .states
            .iter()
            .map(|(k, v)| {
                let mut k: State = k.clone();
                k[i] = new_cards_from(k[i].iter().zip(cards).map(|(a, b)| a + b));
                (k, *v)
            })
            .collect();
    }

    fn remove_cards(&mut self, name: &str, cards: &Hand) {
        self.know_has(name, cards);
        let i = self.get_player_index(name);

        self.states = self
            .states
            .iter()
            .map(|(k, v)| {
                let mut k = k.clone();
                k[i] = new_cards_from(k[i].iter().zip(cards).map(|(a, b)| a - b));
                (k, *v)
            })
            .collect();
    }

    fn add_event(&mut self, event: &[&str]) {
        println!("{} got {}", event[0], event[1]);
        self.add_cards(event[0], &to_cards(event[1]))
    }
    fn discard_event(&mut self, event: &[&str]) {
        println!("{} discarded {}", event[0], event[1]);
        self.remove_cards(event[0], &to_cards(event[1]))
    }
    fn spend_event(&mut self, event: &[&str]) {
        println!("{} purchased {}", event[0], event[1]);
        let cost = match event[1] {
            "road" => [1, 1, 0, 0, 0],
            "settlement" => [1, 1, 1, 1, 0],
            "city" => [0, 0, 0, 2, 3],
            "development card" => [0, 0, 1, 1, 1],
            _ => panic!("Unknown item: {}", event[1]),
        };
        self.remove_cards(event[0], &cost);
    }
    fn rob_event(&mut self, event: &[&str]) {
        println!("{} stole {} from {}", event[0], event[1], event[2]);

        if event[1] == "card" {
            let robber_id = self.get_player_index(event[0]);
            let robbee_id = self.get_player_index(event[2]);

            let mut results = HashMap::new();
            for (state, count) in self.states.iter() {
                for (card_index, num_card) in
                    state[robbee_id].iter().enumerate().filter(|(_, c)| **c > 0)
                {
                    let mut s_new = state.clone();
                    s_new[robber_id][card_index] += 1;
                    s_new[robbee_id][card_index] -= 1;
                    results.insert(s_new, *count * num_card);
                }
            }
            self.states = results;
        } else {
            // rob involving ourselves and we know the card that was taken
            let cards = to_cards(event[1]);
            self.add_cards(event[0], &cards);
            self.remove_cards(event[2], &cards);
        }
    }

    fn handle_trade_offer(&mut self, event: &[&str]) {
        println!("{} offers {} for {}", event[0], event[1], event[2]);
        self.know_has(event[0], &to_cards(event[1]));
    }
    fn handle_trade(&mut self, event: &[&str]) {
        println!(
            "{} traded {} for {} with {}",
            event[0], event[1], event[2], event[3]
        );
        let offer = to_cards(event[1]);
        let want = to_cards(event[2]);
        self.add_cards(event[0], &want);
        self.remove_cards(event[3], &want);
        self.add_cards(event[3], &offer);
        self.remove_cards(event[0], &offer);
    }
    fn handle_year_of_plenty(&mut self, event: &[&str]) {
        println!("{} took from bank {}", event[0], event[1]);
        self.add_cards(event[0], &to_cards(event[1]));
    }
    fn handle_bank_trade(&mut self, event: &[&str]) {
        println!("{} gave bank {} for {}", event[0], event[1], event[2]);
        self.remove_cards(event[0], &to_cards(event[1]));
        self.add_cards(event[0], &to_cards(event[2]));
    }
    fn monopoly_event(&mut self, event: &[&str]) {
        println!("{} monopolied {}", event[0], event[1]);
        let i = self.get_player_index(event[0]);
        let card_index = match event[1] {
            "lumber" => 0,
            "brick" => 1,
            "wool" => 2,
            "grain" => 3,
            "ore" => 4,
            _ => panic!("Unknown resource: {}", event[1]),
        };

        // FIXME: are we summing on the wrong axis?
        let total: u32 = self
            .states
            .keys()
            .next()
            .unwrap()
            .iter()
            .map(|c| c[i])
            .sum();

        let mut results = HashMap::new();
        for (state, count) in self.states.iter() {
            let mut s_new = state.clone();
            for player in 0..N_PLAYERS {
                s_new[player][card_index] = 0;
            }
            s_new[i][card_index] = total;
            results.insert(s_new, *count);
        }
        self.states = results;
    }

    /// Computes the expected value for the number of cards each player has
    fn expected(&self) -> [[f64; N_RESOURCES]; N_PLAYERS] {
        let total = self.states.values().sum::<u32>() as f64;

        let mut expected = [[0.0; N_RESOURCES]; N_PLAYERS];
        for (state, count) in self.states.iter() {
            for (player, cards) in state.iter().enumerate() {
                for (card, num) in cards.iter().enumerate() {
                    expected[player][card] += *num as f64 * *count as f64 / total;
                }
            }
        }
        expected
    }

    /// Computes the minimum number of cards each player could have
    fn sure(&self) -> State {
        let mut sure = self.states.keys().next().unwrap().clone();
        for state in self.states.keys() {
            for (player, cards) in state.iter().enumerate() {
                for (card, num) in cards.iter().enumerate() {
                    sure[player][card] = std::cmp::min(sure[player][card], *num);
                }
            }
        }
        sure
    }

    fn build_table(&self) -> String {
        let mut result = String::new();

        // Header
        result.push_str(&format!(
            format_str!(),
            "Player", "Lumber", "Brick", "Wool", "Grain", "Ore", "Total"
        ));

        let expected = self.expected();
        let sure = self.sure();
        let mut totals = [0.0; N_RESOURCES];

        // Create inverse of player map
        let player_map: HashMap<usize, &str> = self
            .player_idx
            .iter()
            .map(|(i, s)| (*s, i.as_ref()))
            .collect();

        // we're only changing the display value here...
        for id in self.players {
            let player = match player_map.get(&id) {
                Some(&s) => s,
                None => continue,
            };

            totals[0] += expected[id][0];
            totals[1] += expected[id][1];
            totals[2] += expected[id][2];
            totals[3] += expected[id][3];
            totals[4] += expected[id][4];

            result.push_str(&format!(
                format_str!(),
                player,
                format!(
                    "{:>2} ({:.2})",
                    sure[id][0],
                    expected[id][0] - sure[id][0] as f64
                ),
                format!(
                    "{:>2} ({:.2})",
                    sure[id][1],
                    expected[id][1] - sure[id][1] as f64
                ),
                format!(
                    "{:>2} ({:.2})",
                    sure[id][2],
                    expected[id][2] - sure[id][2] as f64
                ),
                format!(
                    "{:>2} ({:.2})",
                    sure[id][3],
                    expected[id][3] - sure[id][3] as f64
                ),
                format!(
                    "{:>2} ({:.2})",
                    sure[id][4],
                    expected[id][4] - sure[id][4] as f64
                ),
                format!("{:>2.0}", expected[id].iter().sum::<f64>())
            ));
        }

        result.push_str(&format!(
            format_str!(),
            "Total",
            format!("{:>2.0}", totals[0]),
            format!("{:>2.0}", totals[1]),
            format!("{:>2.0}", totals[2]),
            format!("{:>2.0}", totals[3]),
            format!("{:>2.0}", totals[4]),
            format!("{:>2.0}", totals.iter().sum::<f64>())
        ));

        result
    }

    /// Parses a command received on cli
    fn parse_command(&mut self, command: &str) {
        // split once or return
        let mut parts = command.split_whitespace();
        let op = match parts.next() {
            Some(op) => op,
            None => return,
        };

        match op {
            "rename" => {
                self.players = parts
                    .map(|p| p.parse::<usize>().expect("Expected integer"))
                    .collect::<Vec<_>>()
                    .try_into()
                    .expect("Expected 4 numbers");
            }
            _ => println!("Unknown command: {}", command),
        }
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut browser = cdp_client::Browser::new("http://localhost:9222/json")?;
    let mut tracker = Tracker::new();

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

// unit tests
#[cfg(test)]
mod tests {
    use super::{Tracker, parse_html};

    #[test]
    fn test_to_cards() {
        use crate::to_cards;
        assert_eq!(to_cards(""), [0, 0, 0, 0, 0]);
        assert_eq!(to_cards("lumber"), [1, 0, 0, 0, 0]);
        assert_eq!(to_cards("brick"), [0, 1, 0, 0, 0]);
        assert_eq!(to_cards("wool"), [0, 0, 1, 0, 0]);
        assert_eq!(to_cards("grain"), [0, 0, 0, 1, 0]);
        assert_eq!(to_cards("ore"), [0, 0, 0, 0, 1]);
        assert_eq!(to_cards("brick lumber"), [1, 1, 0, 0, 0]);
        assert_eq!(to_cards("bricklumber wool"), [1, 1, 1, 0, 0]);
        assert_eq!(to_cards("brick lumber wool grain"), [1, 1, 1, 1, 0]);
        assert_eq!(to_cards(" brick lumber  woolgrain ore "), [1, 1, 1, 1, 1]);
    }

    #[test]
    fn test_html() {
        let mut tracker = Tracker::new();
        let html = std::fs::read_to_string("games/game2.html").unwrap();
        let lines = parse_html(&html);
        tracker.parse_log(&lines);
    }
}
