use std::collections::HashMap;

use ndarray::arr1;
use ndarray::Array0;
use regex::Captures;
// use html_parser::{Dom, Element};
use scraper::{ElementRef, Html, Selector};
use serde_json::{json, Value};
use tungstenite::{connect, Message};

mod browser;
use browser::Browser;
use regex::{Match, Regex};
mod util;

use crate::util::Event;
use crate::util::Resource;

use serde::{Deserialize, Serialize};

const N_PLAYERS: usize = 4;
const N_RESOURCES: usize = 5;
const NAME_PTTN: &str = r"(\w+(?:#\d+)?)";
const CARD_PTTN: &str = r"((?:(?:lumber|brick|wool|grain|ore) ?)+)";
const ITEM_PTTN: &str = r"road|settlement|city|development card";

use ndarray::{s, Array1, Array2};

fn to_cards(cards: &str) -> Array1<u32> {
    let mut result = Array1::zeros(N_RESOURCES);
    for resource in cards
        .split_whitespace()
        .map(|s| Resource::try_from(s).unwrap())
    {
        result[resource as usize] += 1;
    }
    result
}

fn parse_cards(cards: &str) -> Array1<u32> {
    let mut result = Array1::zeros(N_RESOURCES);
    for resource in cards
        .split_whitespace()
        .map(|s| Resource::try_from(s).unwrap())
    {
        result[resource as usize] += 1;
    }
    result
}

struct Tracker {
    players: HashMap<String, usize>,
    states: HashMap<Array2<u32>, u32>,
    events: [(Regex, fn(&mut Self, Vec<&str>) -> ()); 9],
    last_line: usize,
}

impl Tracker {
    fn new() -> Self {
        Self {
            players: HashMap::new(),
            states: HashMap::from([(Array2::zeros((N_PLAYERS, N_RESOURCES)), 1)]),
            events: [
                (
                    Regex::new(&format!(
                        r"{} (?:got|received starting resources): {}",
                        NAME_PTTN, CARD_PTTN
                    ))
                    .unwrap(),
                    Tracker::add_event,
                ),
                (
                    Regex::new(&format!(r"{} discarded: {}", NAME_PTTN, CARD_PTTN)).unwrap(),
                    Tracker::discard_event,
                ),
                (
                    Regex::new(&format!(r"{} (?:built a|bought) ([\w ]+)", NAME_PTTN)).unwrap(),
                    Tracker::spend_event,
                ),
                (
                    Regex::new(&format!(
                        r"{} stole: {} from: {}",
                        NAME_PTTN, CARD_PTTN, NAME_PTTN
                    ))
                    .unwrap(),
                    Tracker::rob_event,
                ),
                (
                    Regex::new(&format!(
                        r"{} wants to give: {} for: {}",
                        NAME_PTTN, CARD_PTTN, CARD_PTTN
                    ))
                    .unwrap(),
                    Tracker::offer_event,
                ),
                (
                    Regex::new(&format!(
                        r"{} traded: {} for: {} with: {}",
                        NAME_PTTN, CARD_PTTN, CARD_PTTN, NAME_PTTN
                    ))
                    .unwrap(),
                    Tracker::trade_event,
                ),
                (
                    Regex::new(&format!(r"{} took from bank: {}", NAME_PTTN, CARD_PTTN)).unwrap(),
                    Tracker::year_of_plenty_event,
                ),
                (
                    Regex::new(&format!(
                        r"{} gave bank: {} and took {}",
                        NAME_PTTN, CARD_PTTN, CARD_PTTN
                    ))
                    .unwrap(),
                    Tracker::bank_trade_event,
                ),
                (
                    Regex::new(&format!(
                        r"{} used monopoly & stole all of: {}",
                        NAME_PTTN, CARD_PTTN
                    ))
                    .unwrap(),
                    Tracker::monopoly_event,
                ),
            ],
            last_line: 0,
        }
    }

    fn get_player_index(&mut self, name: &str) -> usize {
        let i = self.players.len();
        *self.players.entry(name.to_owned()).or_insert(i)
    }

    fn cards_to_ndarray(&mut self, name: &str, cards: &Array1<u32>) -> Array2<u32> {
        let mut state = Array2::zeros((N_PLAYERS, N_RESOURCES));
        let i = self.get_player_index(name);
        state.slice_mut(s![i, ..]).assign(cards);
        state
    }

    fn parse_html(&mut self, html: &str) {
        println!("START");
        let document = Html::parse_document(html);
        let msg_selector = Selector::parse(".message_post").unwrap();
        let img_selector = Selector::parse("img").unwrap();

        for message in document.select(&msg_selector) {
            let mut text = message.inner_html();
            for img in message.select(&img_selector) {
                let alt_text = format!("{} ", img.value().attr("alt").unwrap());
                text = text.replace(&img.html(), &alt_text);
            }

            for (pattern, func) in self.events.iter() {
                if let Some(captures) = pattern.captures(&text) {
                    let args = captures
                        .iter()
                        .skip(1)
                        .map(|c| c.unwrap().as_str())
                        .collect::<Vec<_>>();
                    func(self, args);
                    break;
                }
            }
        }
        println!("END");

        println!("{:?}", self.states);
    }

    fn add_cards(&mut self, name: &str, cards: &Array1<u32>) {
        let state = self.cards_to_ndarray(name, cards);
        self.states = self
            .states
            .iter()
            .map(|(k, v)| (k + state.to_owned(), v.to_owned()))
            .collect();
    }

    fn remove_cards(&mut self, name: &str, cards: &Array1<u32>) {
        let state = self.cards_to_ndarray(name, cards);

        let z: Array1<u32> = arr1(&[1, 0, 1]) - arr1(&[0, 1, 0]);

        // let z: HashMap<Array2<u32>, u32> = HashMap::new()
        //     .into_iter()
        //     .map(|(k, v): (Array2<u32>, u32)| (k.checked_sub(state), v))
        //     .flatten()
        //     .collect();

        // self.states = self
        //     .states
        //     .iter()
        //     .map(|(k, v)| (k.checked_sub(state.to_owned()), v.to_owned()))
        //     .filter(|(k, _)| k.iter().any(|&x| x != 0))
        //     .collect();
    }

    fn add_event(&mut self, event: Vec<&str>) {
        println!("Add event: {}, {}", event[0], event[1]);
        self.add_cards(event[0], &to_cards(event[1]))
    }
    fn discard_event(&mut self, event: Vec<&str>) {
        println!("Discard event: {}, {}", event[0], event[1]);
        self.remove_cards(event[0], &to_cards(event[1]))
    }
    fn spend_event(&mut self, event: Vec<&str>) {
        println!("Spend event: {}, {}", event[0], event[1]);
    }
    fn rob_event(&mut self, event: Vec<&str>) {
        println!("Rob event: {}, {}", event[0], event[1]);
    }
    fn offer_event(&mut self, event: Vec<&str>) {
        println!("Offer event: {}, {}", event[0], event[1]);
    }
    fn trade_event(&mut self, event: Vec<&str>) {
        println!("Trade event: {}, {}", event[0], event[1]);
    }
    fn year_of_plenty_event(&mut self, event: Vec<&str>) {
        println!("Year of Plenty event: {}, {}", event[0], event[1]);
    }
    fn bank_trade_event(&mut self, event: Vec<&str>) {
        println!("Bank Trade event: {}, {}", event[0], event[1]);
    }
    fn monopoly_event(&mut self, event: Vec<&str>) {
        println!("Monopoly event: {}, {}", event[0], event[1]);
    }

    fn expected(self) -> Array2<f64> {
        let total: u32 = self.states.values().sum();

        let product = self
            .states
            .iter()
            .map(|(k, v)| k.clone() * *v)
            .fold(Array2::<u32>::zeros((N_PLAYERS, N_RESOURCES)), |acc, x| {
                acc + x
            });
        product.mapv(|x| x as f64) / total as f64
    }
}

pub type Root1 = Vec<Root2>;

#[derive(Default, Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Root2 {
    pub web_socket_debugger_url: String,
}

// fn parse_message(message: &str) -> Option<Event> {
//     // clean up the message, ensure no extra whitespace
//     let message = Regex::new(r"\s\s+").unwrap().replace_all(message, " ").trim().to_owned();

//     lazy_static! {
//         static ref recv_pttn: Regex = Regex::new(&format!(r"(?:got|received starting resources): {}", CARD_PTTN)).unwrap();
//         static ref drop_pttn: Regex = Regex::new(&format!(r"discarded: {}", CARD_PTTN)).unwrap();
//         static ref spend_pttn: Regex = Regex::new(&format!(r"(?:built a|bought) {}", ITEM_PTTN)).unwrap();
//     }

//     // println!("MESSAGE: {}", message);

//     // if let Some(name) = message.split_whitespace().skip(1).next().map(|s| s.to_owned()) {

//     //     if let Some(captures) = recv_pttn.captures(&message) {
//     //         let cards = to_cards(captures.get(1).unwrap().as_str());
//     //         Some(Event::Receive(name, cards))
//     //     } else if let Some(captures) = drop_pttn.captures(&message) {
//     //         let cards = to_cards(captures.get(1).unwrap().as_str());
//     //         Some(Event::Discard(name, cards))
//     //     } else {
//     //         None
//     //     }
//     // } else {
//     //     None
//     // }

// }

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // let mut browser = Browser::new()?;

    // let response = browser.send(
    //     "Runtime.evaluate",
    //     json!({
    //         "expression": "document.getElementById(\"game-log-text\").outerHTML"
    //     }),
    // )?;

    // if let Value::String(html) = &response["result"]["value"] {
    //     std::fs::write("test.html", html)?;
    // }

    let a: Array1<u32> = arr1(&[1, 0, 1]);
    let b: Array1<u32> = arr1(&[0, 0, 0]);

    let z = a.iter().zip(b.iter()).map(|(x, y)| x < y).any(|x| x);
    println!("{}", z);



    // let x = std::fs::read_to_string("test.html")?;

    // let z = Tracker::new().parse_html(&x);

    // println!("{:?}", expected);

    Ok(())
}
