from typing import List
from enum import Enum
import random
from functools import reduce

class OutOfRangeException(Exception):
  pass

class Card:
  suit: int
  number: int

  def __init__(self, num: int, suit: int):
    if num < 1 or 13 < num:
      raise OutOfRangeException("num should be 1~13")

    if suit < 0 or 3 < suit:
      raise OutOfRangeException("suit should be 0~3")

    self.number = num
    self.suit = suit

  def toSuitName(self):
    suit_names = {0: "Spade"
                 , 1: "Club"
                 , 2: "Heart"
                 , 3: "Diamond"}
    return suit_names[self.suit]

  def toCardName(self):
    special_names = {11: "J"
                     , 12: "Q"
                     , 13: "K"
                     , 1: "A"
                     }
    return str(self.number) if self.number not in special_names else special_names[self.number]

  def __str__(self) -> str:
    return f"{self.toCardName()} of {self.toSuitName()}"


  def disp(self) -> str:
    return self.__str__()




class Deck:
  def __init__(self):
    self.cards = [Card(n, suit) for n in range(1, 14) for suit in range(4)]
    random.shuffle(self.cards)
    

  def draw(self) -> Card:
    return self.cards.pop()

class Suit(Enum):
  Spade   = 0
  Club    = 1
  Heart   = 2
  Diamond = 3

class Player:
  cards: List[Card]
  name: str

  def __init__(self, cards: List[Card], name=None):
    self.cards = cards
    self.name = name if name is not None else random.choice(["alice", "bob"])


  def __str__(self) -> str:
    return f"{self.name}(total: {self.total()})"

    
  def hit(self, deck: Deck):
    """ Hit new card from deck and store it
    """
    self.cards.append(deck.draw())

  def show_cards(self) -> str:
    """ Human readable representation of cards currently have
    """
    return reduce(lambda a, b: str(a) + ', ' + str(b), self.cards)

  def total(self) -> int:
    """ Total number of cards
    """
    ace_as_1 = sum([c.number for c in self.cards])
    ace_as_11 = sum(list(map(lambda n: 11 if n == 1 else n, [c.number for c in self.cards])))
    return ace_as_1 if (21 < ace_as_11) or abs(21-ace_as_1) < abs(21-ace_as_11) else ace_as_11
  
  def is_busted(self):
    return 21 < self.total()


class BlackJack:
  dealer: Player
  players: List[Player]
  deck: Deck

  def run(self):
    self.deck = Deck()
    self.dealer = Player([self.deck.draw() for _ in range(2)], name="Dealer")
    self.players = [Player([self.deck.draw() for _ in range(2)])]


    print(f"One of dealer's card is: {self.dealer.cards[0]}")
    for p in self.players:
      self.player_loop(p)

    self.dealer_loop()
    print(f"winner players are: {self.get_winner_players()}")

  def player_loop(self, player: Player):
    """ Player's one turn
    
        Player can choose hit or stand,
        unless they're busted already.
    """
    print(f"Your cards are: {player.show_cards()}")
    cmd = input("Hit?(h = hit, s = stand): ")
    if cmd == "h":
      player.hit(self.deck)
      if not player.is_busted():
        self.player_loop(player)
    elif cmd == "s":
      pass

  def dealer_loop(self):
    """ Dealer's one turn
    
        Dealer should hit when:
        - His total of cards is less than 17

        And should NOT hit otherwise.
    """
    print(f"Dealer's cards are: {self.dealer.show_cards()}")
    if self.dealer.total() < 17:
      self.dealer.hit(self.deck)
      self.dealer_loop()

  def get_winner_players(self) -> List[Player]:
    """ Returns list of all players that won the game
    
        Note: As blackjack is 1vs1 card game, 
              dealer won against all other players not listed here.
    """
    alive_players = filter(lambda p: not p.is_busted(), self.players)

    # When dealer is busted, all players who isn't busted will win
    if self.dealer.is_busted():
      return list(alive_players)

    return list(filter(lambda p: p.total() > self.dealer.total(), alive_players))

  def mkResult(self) -> str:
    """ Make one string that reports result of the game
    """
    return ""
    

BlackJack().run()
