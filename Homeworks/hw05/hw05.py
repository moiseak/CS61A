# You do not need to understand what this function does. It is used for testing.
def make_test_random():
    """A deterministic random function that cycles between
    [0.0, 0.1, 0.2, ..., 0.9] for testing purposes.

    >>> random = make_test_random()
    >>> random()
    0.0
    >>> random()
    0.1
    >>> random2 = make_test_random()
    >>> random2()
    0.0
    """
    rands = [x / 10 for x in range(10)]
    def random():
        rand = rands[0]
        rands.append(rands.pop(0))
        return rand
    return random

### Phase 1: The Player Class
class Player:
    """
    >>> random = make_test_random()
    >>> p1 = Player('Hill', random)
    >>> p2 = Player('Don', random)
    >>> p1.popularity
    100
    >>> p1.debate(p2)  # random() should return 0.0
    >>> p1.popularity
    150
    >>> p2.popularity
    100
    >>> p2.votes
    0
    >>> p2.speech(p1)
    >>> p2.votes
    10
    >>> p2.popularity
    110
    >>> p1.popularity
    135
    >>> p1.speech(p2)
    >>> p1.votes
    13
    >>> p1.popularity
    148
    >>> p2.votes
    10
    >>> p2.popularity
    99
    >>> for _ in range(4):  # 0.1, 0.2, 0.3, 0.4
    ...     p1.debate(p2)
    >>> p2.debate(p1)
    >>> p2.popularity
    49
    >>> p2.debate(p1)
    >>> p2.popularity
    0
    """
    def __init__(self, name, random_func):
        self.name = name
        self.votes = 0
        self.popularity = 100
        self.random_func = random_func

    def debate(self, other):
        "*** YOUR CODE HERE ***"

    def speech(self, other):
        "*** YOUR CODE HERE ***"

    def choose(self, other):
        return self.speech


### Phase 2: The Game Class
class Game:
    """
    >>> random = make_test_random()
    >>> p1, p2 = Player('Hill',random), Player('Don', random)
    >>> g = Game(p1, p2)
    >>> winner = g.play()
    >>> p1 is winner
    True
    >>> # Additional correctness tests
    >>> winner is g.winner()
    True
    >>> g.turn
    10
    >>> p1.votes = p2.votes
    >>> print(g.winner())
    None
    """
    def __init__(self, player1, player2):
        self.p1 = player1
        self.p2 = player2
        self.turn = 0

    def play(self):
        while not self.game_over():
            "*** YOUR CODE HERE ***"
        return self.winner()

    def game_over(self):
        return max(self.p1.votes, self.p2.votes) >= 50 or self.turn >= 10

    def winner(self):
        "*** YOUR CODE HERE ***"


### Phase 3: New Players
class AggressivePlayer(Player):
    """
    >>> random = make_test_random()
    >>> p1, p2 = AggressivePlayer('Don', random), Player('Hill', random)
    >>> g = Game(p1, p2)
    >>> winner = g.play()
    >>> p1 is winner
    True
    >>> # Additional correctness tests
    >>> p1.popularity = p2.popularity
    >>> p1.choose(p2) == p1.debate
    True
    >>> p1.popularity += 1
    >>> p1.choose(p2) == p1.debate
    False
    >>> p2.choose(p1) == p2.speech
    True
    """
    def choose(self, other):
        "*** YOUR CODE HERE ***"

class CautiousPlayer(Player):
    """
    >>> random = make_test_random()
    >>> p1, p2 = CautiousPlayer('Hill', random), AggressivePlayer('Don', random)
    >>> p1.popularity = 0
    >>> p1.choose(p2) == p1.debate
    True
    >>> p1.popularity = 1
    >>> p1.choose(p2) == p1.debate
    False
    >>> # Additional correctness tests
    >>> p2.choose(p1) == p2.speech
    True
    """
    def choose(self, other):
        "*** YOUR CODE HERE ***"


def add_d_leaves(t, v):
    """Add d leaves containing v to each node at every depth d.

    >>> t_one_to_four = Tree(1, [Tree(2), Tree(3, [Tree(4)])])
    >>> print(t_one_to_four)
    1
      2
      3
        4
    >>> add_d_leaves(t_one_to_four, 5)
    >>> print(t_one_to_four)
    1
      2
        5
      3
        4
          5
          5
        5

    >>> t0 = Tree(9)
    >>> add_d_leaves(t0, 4)
    >>> t0
    Tree(9)
    >>> t1 = Tree(1, [Tree(3)])
    >>> add_d_leaves(t1, 4)
    >>> t1
    Tree(1, [Tree(3, [Tree(4)])])
    >>> t2 = Tree(2, [Tree(5), Tree(6)])
    >>> t3 = Tree(3, [t1, Tree(0), t2])
    >>> print(t3)
    3
      1
        3
          4
      0
      2
        5
        6
    >>> add_d_leaves(t3, 10)
    >>> print(t3)
    3
      1
        3
          4
            10
            10
            10
          10
          10
        10
      0
        10
      2
        5
          10
          10
        6
          10
          10
        10
    """
    "*** YOUR CODE HERE ***"


def level_mutation_link(t, funcs):
	"""Mutates t using the functions in the linked list funcs.

	>>> t = Tree(1, [Tree(2, [Tree(3)])])
	>>> funcs = Link(lambda x: x + 1, Link(lambda y: y * 5, Link(lambda z: z ** 2)))
	>>> level_mutation_link(t, funcs)
	>>> t    # At level 0, apply x + 1; at level 1, apply y * 5; at level 2 (leaf), apply z ** 2
	Tree(2, [Tree(10, [Tree(9)])])
	>>> t2 = Tree(1, [Tree(2), Tree(3, [Tree(4)])])
	>>> level_mutation_link(t2, funcs)
	>>> t2    # Level 0: 1+1=2; Level 1: 2*5=10 => 10**2 = 100, 3*5=15; Level 2 (leaf): 4**2=16
	Tree(2, [Tree(100), Tree(15, [Tree(16)])])
	>>> t3 = Tree(1, [Tree(2)])
	>>> level_mutation_link(t3, funcs)
	>>> t3    # Level 0: 1+1=2; Level 1: 2*5=10; no further levels, so apply remaining z ** 2: 10**2=100
	Tree(2, [Tree(100)])
	"""
	if _____________________:
		return
	t.label = _____________________
	remaining = _____________________
	if __________________ and __________________:
		while _____________________:
			_____________________
			remaining = remaining.rest
	for b in t.branches:
		_____________________


def store_digits(n):
    """Stores the digits of a positive number n in a linked list.

    >>> s = store_digits(1)
    >>> s
    Link(1)
    >>> store_digits(2345)
    Link(2, Link(3, Link(4, Link(5))))
    >>> store_digits(876)
    Link(8, Link(7, Link(6)))
    >>> store_digits(2450)
    Link(2, Link(4, Link(5, Link(0))))
    >>> # a check for restricted functions
    >>> import inspect, re
    >>> cleaned = re.sub(r"#.*\\n", '', re.sub(r'"{3}[\s\S]*?"{3}', '', inspect.getsource(store_digits)))
    >>> print("Do not use str or reversed!") if any([r in cleaned for r in ["str", "reversed"]]) else None
    """
    "*** YOUR CODE HERE ***"


def deep_map_mut(func, lnk):
    """Mutates a deep link lnk by replacing each item found with the
    result of calling func on the item. Does NOT create new Links (so
    no use of Link's constructor).

    Does not return the modified Link object.

    >>> link1 = Link(3, Link(Link(4), Link(5, Link(6))))
    >>> print(link1)
    <3 <4> 5 6>
    >>> # Disallow the use of making new Links before calling deep_map_mut
    >>> Link.__init__, hold = lambda *args: print("Do not create any new Links."), Link.__init__
    >>> try:
    ...     deep_map_mut(lambda x: x * x, link1)
    ... finally:
    ...     Link.__init__ = hold
    >>> print(link1)
    <9 <16> 25 36>
    """
    "*** YOUR CODE HERE ***"


def crispr_gene_insertion(lnk_of_genes, insert):
    """Takes a linked list of genes and mutates the genes with the INSERT codon added the correct number of times.

    >>> link = Link(Link("AUG", Link("GCC", Link("ACG"))), Link(Link("ATG", Link("AUG", Link("ACG", Link("GCC"))))))
    >>> print(link)
    <<AUG GCC ACG> <ATG AUG ACG GCC>>
    >>> crispr_gene_insertion(link, "TTA")
    >>> print(link)
    <<AUG TTA GCC ACG> <ATG AUG TTA TTA ACG GCC>>
    >>> link = Link(Link("ATG"), Link(Link("AUG", Link("AUG")), Link(Link("AUG", Link("GCC")))))
    >>> print(link)
    <<ATG> <AUG AUG> <AUG GCC>>
    >>> crispr_gene_insertion(link, "TTA") # first gene has no AUG so unchanged, 2nd gene has 2 AUGs but only first considered for insertion
    >>> print(link)
    <<ATG> <AUG TTA TTA AUG> <AUG TTA TTA TTA GCC>>
    >>> link = Link.empty # empty linked list of genes stays empty
    >>> crispr_gene_insertion(link, "TTA")
    >>> print(link)
    ()
    """
    "*** YOUR CODE HERE ***"

def transcribe(dna):
    """Takes a string of DNA and returns a Python list with the RNA codons.

    >>> DNA = "TACCTAGCCCATAAA"
    >>> transcribe(DNA)
    ['AUG', 'GAU', 'CGG', 'GUA', 'UUU']
    """
    dict = {'A': 'U', 'T': 'A', 'G': 'C', 'C': 'G'}
    return __________________


class Tree:
    """
    >>> t = Tree(3, [Tree(2, [Tree(5)]), Tree(4)])
    >>> t.label
    3
    >>> t.branches[0].label
    2
    >>> t.branches[1].is_leaf()
    True
    """
    def __init__(self, label, branches=[]):
        for b in branches:
            assert isinstance(b, Tree)
        self.label = label
        self.branches = list(branches)

    def is_leaf(self):
        return not self.branches

    def __repr__(self):
        if self.branches:
            branch_str = ', ' + repr(self.branches)
        else:
            branch_str = ''
        return 'Tree({0}{1})'.format(self.label, branch_str)

    def __str__(self):
        def print_tree(t, indent=0):
            tree_str = '  ' * indent + str(t.label) + "\n"
            for b in t.branches:
                tree_str += print_tree(b, indent + 1)
            return tree_str
        return print_tree(self).rstrip()


class Link:
    """A linked list.

    >>> s = Link(1)
    >>> s.first
    1
    >>> s.rest is Link.empty
    True
    >>> s = Link(2, Link(3, Link(4)))
    >>> s.first = 5
    >>> s.rest.first = 6
    >>> s.rest.rest = Link.empty
    >>> s                                    # Displays the contents of repr(s)
    Link(5, Link(6))
    >>> s.rest = Link(7, Link(Link(8, Link(9))))
    >>> s
    Link(5, Link(7, Link(Link(8, Link(9)))))
    >>> print(s)                             # Prints str(s)
    <5 7 <8 9>>
    """
    empty = ()

    def __init__(self, first, rest=empty):
        assert rest is Link.empty or isinstance(rest, Link)
        self.first = first
        self.rest = rest

    def __repr__(self):
        if self.rest is not Link.empty:
            rest_repr = ', ' + repr(self.rest)
        else:
            rest_repr = ''
        return 'Link(' + repr(self.first) + rest_repr + ')'

    def __str__(self):
        string = '<'
        while self.rest is not Link.empty:
            string += str(self.first) + ' '
            self = self.rest
        return string + str(self.first) + '>'

