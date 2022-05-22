use enum_map::EnumMap;
use regex::Regex;
use scraper::{Html, Selector};
use serde_json::{json, Value};
use std::collections::HashMap;
mod util;

use crate::util::input;
use util::{possible_hands, Hand, Resource, State, N_PLAYERS};

const NAME: &str = r"(\w+(?:#\d+)?)";
const CARDS: &str = r"((?:(?:lumber|brick|wool|grain|ore|card) ?)+)";
const ITEM_PTTN: &str = r"(road|settlement|city|development card)";
const USERNAME: &str = "Mera#4025";

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

/// Parses cards with any amount of whitespace between them.
fn to_cards(cards: &str) -> Hand {
    let re = Regex::new(r"(lumber|brick|wool|grain|ore)").unwrap();

    let mut result = Hand::default();
    for capture in re.captures_iter(cards) {
        let text = capture.get(0).unwrap().as_str();
        let card = Resource::try_from(text).unwrap();
        result[card] += 1;
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
            Tracker::handle_receive,
        ),
        (
            Regex::new(&format!(r"{NAME} discarded: *{CARDS}")).unwrap(),
            Tracker::handle_discard,
        ),
        (
            Regex::new(&format!(r"{NAME} (?:built a|bought) {ITEM_PTTN}")).unwrap(),
            Tracker::handle_purchase,
        ),
        (
            Regex::new(&format!(r"{NAME} stole:? {CARDS} from:? {NAME}")).unwrap(),
            Tracker::handle_rob,
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
    pub fn parse_log<T: AsRef<str> + std::fmt::Debug>(&mut self, messages: &[T]) {
        if (self.last_line == 0 || self.last_line > messages.len())
            && !messages[0].as_ref().starts_with("List of Commands: /help")
        {
            if messages.len() != 1 {
                println!("\n### REFRESH PAGE TO RESET CHAT ###\n");
                return;
            }
            self.handle_reset();
            self.last_line = 0;
        }

        for line in &messages[self.last_line..] {
            let line = normalize_text(line.as_ref());
            for (regex, event) in &self.events {
                if let Some(caps) = regex.captures(line.as_ref()) {
                    let args = caps
                        .iter()
                        .skip(1)
                        .map(|m| m.unwrap().as_str())
                        .collect::<Vec<_>>();
                    event(self, &args);
                    break; // there should only be one event to parse per line
                }
            }
        }
        self.last_line = messages.len();
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
        self.states.retain(|state, _| {
            state[i]
                .iter()
                .zip(cards.iter())
                .all(|((_, a), (_, b))| *a >= *b)
        });
        assert!(self.states.len() > 0);
    }

    /// Adds a `Hand` of cards to every state for a player
    fn add_cards(&mut self, name: &str, cards: &Hand) {
        let i = self.get_player_index(name);
        self.states = self
            .states
            .iter()
            .map(|(k, v)| {
                let mut k = k.clone();
                for (card, count) in cards {
                    k[i][card] += count;
                }
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
                for (card, count) in cards {
                    k[i][card] -= count;
                }
                (k, *v)
            })
            .collect();
    }

    fn handle_receive(&mut self, event: &[&str]) {
        println!("{} got {}", event[0], event[1]);
        self.add_cards(event[0], &to_cards(event[1]))
    }
    fn handle_discard(&mut self, event: &[&str]) {
        println!("{} discarded {}", event[0], event[1]);
        self.remove_cards(event[0], &to_cards(event[1]))
    }
    fn handle_purchase(&mut self, event: &[&str]) {
        println!("{} purchased {}", event[0], event[1]);
        let cost = match event[1] {
            "road" => Hand::from_array([1, 1, 0, 0, 0]),
            "settlement" => Hand::from_array([1, 1, 1, 1, 0]),
            "city" => Hand::from_array([0, 0, 0, 2, 3]),
            "development card" => Hand::from_array([0, 0, 1, 1, 1]),
            _ => panic!("Unknown item: {}", event[1]),
        };
        self.remove_cards(event[0], &cost);
    }
    fn handle_rob(&mut self, event: &[&str]) {
        println!("{} stole {} from {}", event[0], event[1], event[2]);

        if event[1] == "card" {
            // we don't know what card was stolen
            let robber_id = self.get_player_index(event[0]);
            let robbee_id = self.get_player_index(event[2]);

            let mut results = HashMap::new();
            for (state, count) in self.states.iter() {
                for (card, num) in state[robbee_id].iter().filter(|(_, c)| **c > 0) {
                    let mut s_new = state.clone();
                    s_new[robber_id][card] += 1;
                    s_new[robbee_id][card] -= 1;
                    results.insert(s_new, *num as u32 * count);
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

        let card = match Resource::try_from(event[1]) {
            Ok(card) => card,
            Err(_) => panic!("Unknown resource: {}", event[1]),
        };

        // FIXME: are we summing on the wrong axis?
        let total = self
            .states
            .keys()
            .next()
            .unwrap() // there should be at least one state
            .iter()
            .map(|c| c[card])
            .sum();

        self.states = self
            .states
            .iter()
            .map(|(state, v)| {
                let mut state = state.clone();
                for mut hand in state {
                    hand[card] = 0;
                }
                state[i][card] = total;
                (state, *v)
            })
            .collect();
    }

    /// Computes the expected value for the number of cards each player has
    fn expected(&self) -> [EnumMap<Resource, f32>; N_PLAYERS] {
        let n_states = self.states.values().sum::<u32>() as f32;
        let mut expected = <[EnumMap<Resource, f32>; N_PLAYERS]>::default();
        for (state, count) in self.states.iter() {
            for (player, cards) in state.iter().enumerate() {
                for (card, num) in cards.iter() {
                    expected[player][card] += *num as f32 * *count as f32 / n_states;
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
                for (card, num) in cards.iter() {
                    sure[player][card] = std::cmp::min(sure[player][card], *num);
                }
            }
        }
        sure
    }

    /// Iterates players in view order
    fn in_order(&self) -> impl Iterator<Item = (usize, &str)> {
        let player_map: HashMap<usize, &str> = self
            .player_idx
            .iter()
            .map(|(i, s)| (*s, i.as_ref()))
            .collect();

        self.players
            .into_iter()
            .filter_map(move |id| match player_map.get(&id) {
                Some(player) => Some((id, *player)),
                None => None,
            })
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
        let mut totals = EnumMap::<Resource, f32>::default();

        for (id, player) in self.in_order() {
            for (card, num) in expected[id].into_iter() {
                totals[card] += num;
            }

            result.push_str(&format!(
                format_str!(),
                player,
                format!(
                    "{:>2} ({:1.2})",
                    sure[id][Resource::Lumber],
                    expected[id][Resource::Lumber] - sure[id][Resource::Lumber] as f32
                ),
                format!(
                    "{:>2} ({:1.2})",
                    sure[id][Resource::Brick],
                    expected[id][Resource::Brick] - sure[id][Resource::Brick] as f32
                ),
                format!(
                    "{:>2} ({:1.2})",
                    sure[id][Resource::Wool],
                    expected[id][Resource::Wool] - sure[id][Resource::Wool] as f32
                ),
                format!(
                    "{:>2} ({:1.2})",
                    sure[id][Resource::Grain],
                    expected[id][Resource::Grain] - sure[id][Resource::Grain] as f32
                ),
                format!(
                    "{:>2} ({:1.2})",
                    sure[id][Resource::Ore],
                    expected[id][Resource::Ore] - sure[id][Resource::Ore] as f32
                ),
                format!("{:>2.0}", expected[id].values().sum::<f32>())
            ));
        }

        result.push_str(&format!(
            format_str!(),
            "Total",
            format!("{:>2.0}", totals[Resource::Lumber]),
            format!("{:>2.0}", totals[Resource::Brick]),
            format!("{:>2.0}", totals[Resource::Wool]),
            format!("{:>2.0}", totals[Resource::Grain]),
            format!("{:>2.0}", totals[Resource::Ore]),
            format!("{:>2.0}", totals.values().sum::<f32>())
        ));

        result
    }

    /// counts must be in same order as player_idx
    fn reset(&mut self, my_cards: Hand, counts: &[u8; N_PLAYERS - 1]) {
        let mut pools: [Vec<Hand>; N_PLAYERS - 1] = Default::default();
        for (i, count) in counts.iter().enumerate() {
            pools[i] = possible_hands(*count);
        }

        let mut results = HashMap::new();

        // cartesian product of all possible states
        let mut indices = [0; N_PLAYERS - 1];
        'outer: loop {
            // build current state
            let mut state = State::default();
            for (i, value) in indices
                .iter()
                .zip(pools.iter())
                .map(|(i, p)| p[*i])
                .enumerate()
            {
                state[i] = value;
            }
            state[N_PLAYERS - 1] = my_cards;
            results.insert(state, 1);

            // "carry" logic
            for i in (0..pools.len()).rev() {
                indices[i] += 1;
                if indices[i] < pools[i].len() {
                    break;
                }
                indices[i] = 0;
                if i == 0 {
                    break 'outer;
                }
            }
        }
        self.states = results;
    }

    fn handle_reset(&mut self) {
        println!("Can't see start of game. Resetting...");
        self.player_idx = HashMap::new();

        println!("Card counts: [Name count]+");
        let parts = input("> ")
            .split_whitespace()
            .map(|s| s.to_string())
            .collect::<Vec<_>>();

        let mut counts = [0; N_PLAYERS - 1];
        for chunk in parts.chunks(2) {
            let i = self.get_player_index(&chunk[0]);
            counts[i] = chunk[1].parse().unwrap();
        }
        self.get_player_index(USERNAME);

        // get my cards
        println!("Your card counts: lumber brick wool grain ore");
        let my_cards = Hand::from_array(
            input("> ")
                .split_whitespace()
                .map(|s| s.parse::<u8>().expect("Expected u8"))
                .collect::<Vec<_>>()
                .try_into()
                .expect("Expected 5 u8"),
        );

        self.reset(my_cards, &counts);
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
            "states" => {
                println!("States: {}", self.states.len());
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
    use crate::USERNAME;

    use super::{parse_html, Hand, Tracker};

    #[test]
    fn test_html() {
        let mut tracker = Tracker::new();
        let html = std::fs::read_to_string("games/game2.html").unwrap();
        let lines = parse_html(&html);
        tracker.parse_log(&lines);
    }

    #[test]
    fn test_rob() {
        let mut tracker = Tracker::new();
        tracker.add_cards("a", &Hand::from_array([5, 7, 9, 13, 15]));
        tracker.add_cards("b", &Hand::from_array([12, 11, 6, 5, 3]));

        tracker.handle_rob(&["b", "card", "a"]);
        tracker.handle_rob(&["b", "card", "a"]);
        tracker.handle_rob(&["b", "card", "a"]);
        tracker.handle_rob(&["a", "card", "b"]);
        tracker.handle_rob(&["a", "card", "b"]);
        tracker.handle_rob(&["a", "card", "b"]);

        println!("{}", tracker.build_table());
        println!("{}", tracker.states.len());
        assert_eq!(tracker.states.len(), 471);
    }

    #[test]
    fn test_reset() {
        let mut tracker = Tracker::new();
        tracker.add_cards("a", &Hand::from_array([0, 0, 0, 0, 0]));
        tracker.add_cards("b", &Hand::from_array([0, 0, 0, 0, 0]));
        tracker.add_cards("c", &Hand::from_array([0, 0, 0, 0, 0]));
        tracker.add_cards(USERNAME, &Hand::from_array([0, 0, 0, 0, 0]));

        let counts = [3, 5, 2];
        tracker.reset(Hand::from_array([1, 3, 1, 0, 0]), &counts);

        assert_eq!(tracker.states.len(), 66_150);
        println!("{}", tracker.states.len());
    }

    #[test]
    fn test_monopoly() {
        let mut tracker = Tracker::new();
        tracker.add_cards("a", &Hand::from_array([0, 0, 0, 0, 0]));
        tracker.add_cards("b", &Hand::from_array([0, 0, 0, 0, 0]));
        tracker.add_cards("c", &Hand::from_array([0, 0, 0, 0, 0]));
        tracker.add_cards(USERNAME, &Hand::from_array([0, 0, 0, 0, 0]));
    }
}
