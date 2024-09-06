LAB_SOURCE_FILE=__file__


def double_eights(n):
    """ Returns whether n has two digits in row that
    are the number 8. Assume n has at least two digits in it.

    # >>> double_eights(1288)
    # True
    # >>> double_eights(880)
    # True
    # >>> double_eights(538835)
    # True
    # >>> double_eights(284682)
    # False
    # >>> double_eights(588138)
    # True
    # >>> double_eights(78)
    # False
    # >>> from construct_check import check
    # >>> # ban iteration
    # >>> check(LAB_SOURCE_FILE, 'double_eights', ['While', 'For'])
    True
    """
    "*** YOUR CODE HERE ***"
    if n == 0:
        return False
    else:
        if n % 10 == 8 and n // 10 % 10 ==8:
            return True
        return double_eights(n // 10)


def make_onion(f, g):
    """Return a function can_reach(x, y, limit) that returns
    whether some call expression containing only f, g, and x with
    up to limit calls will give the result y.

    # >>> up = lambda x: x + 1
    # >>> double = lambda y: y * 2
    # >>> can_reach = make_onion(up, double)
    # >>> can_reach(5, 25, 4)      # 25 = up(double(double(up(5))))
    # True
    # >>> can_reach(5, 25, 3)      # Not possible
    # False
    # >>> can_reach(1, 1, 0)      # 1 = 1
    # True
    # >>> add_ing = lambda x: x + "ing"
    # >>> add_end = lambda y: y + "end"
    # >>> can_reach_string = make_onion(add_ing, add_end)
    # >>> can_reach_string("cry", "crying", 1)      # "crying" = add_ing("cry")
    # True
    # >>> can_reach_string("un", "unending", 3)     # "unending" = add_ing(add_end("un"))
    # True
    # >>> can_reach_string("peach", "folding", 4)   # Not possible
    False
    """
    def can_reach(x, y, limit):
        if limit < 0:
            return False
        elif x == y:
            return True
        else:
            return can_reach(f(x), y, limit - 1) or can_reach(g(x), y, limit - 1)
    return can_reach


def mario_number(level):
    """
    Return the number of ways that Mario can perform a sequence of steps
    or jumps to reach the end of the level without ever landing in a Piranha
    plant. Assume that every level begins and ends with a space.

    # >>> mario_number(' P ')   # jump, jump
    # 1
    # >>> mario_number(' P P  ')   # jump, jump, step
    # 1
    # >>> mario_number('  P P ')  # step, jump, jump
    # 1
    # >>> mario_number('   P P ') # step, step, jump, jump or jump, jump, jump
    # 2
    # >>> mario_number(' P PP ')  # Mario cannot jump two plants
    # 0
    # >>> mario_number('    ')    # step, jump ; jump, step ; step, step, step
    # 3
    # >>> mario_number('    P    ')
    # 9
    # >>> mario_number('   P    P P   P  P P    P     P ')
    # 180
    # """
    "*** YOUR CODE HERE ***"
    def ways(n):
        if n == len(level) - 1:
            return 1
        elif n >= len(level) or level[n] == 'P':
            return 0
        else:
            return ways(n + 1) + ways(n + 2)
    return ways(0)





def max_subseq(n, t):
    """
    Return the maximum subsequence of length at most t that can be found in the given number n.
    For example, for n = 2012 and t = 2, we have that the subsequences are
        2
        0
        1
        2
        20
        21
        22
        01
        02
        12
    and of these, the maximum number is 22, so our answer is 22.

     max_subseq(2012, 2)
    22
    # >>> max_subseq(20125, 3)
    # 225
    # >>> max_subseq(20125, 5)
    # 20125
    # >>> max_subseq(20125, 6) # note that 20125 == 020125
    # 20125
    # >>> max_subseq(12345, 3)
    # 345
    # >>> max_subseq(12345, 0) # 0 is of length 0
    # 0
    # >>> max_subseq(12345, 1)
    5
    """
    "*** YOUR CODE HERE ***"
    if n == 0 or t == 0:
        return 0
    with_last = max_subseq(n // 10, t - 1) + n % 10
    without_last = max_subseq(n // 10, t)
    return max(without_last, with_last)


def is_prime(n):
    """
    # >>> is_prime(7)
    # True
    # >>> is_prime(10)
    # False
    # >>> is_prime(1)
    False
    """
    "*** YOUR CODE HERE ***"
    def prime(x):
        if x == n:
            return True
        if n % x == 0:
            return False
        else:
            prime(x + 1)
    return prime(2)



