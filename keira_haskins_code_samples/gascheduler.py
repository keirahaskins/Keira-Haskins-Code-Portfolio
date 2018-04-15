"""
@group: 
@author: Keira Haskins
@email:
@author: Ben Mixon-Baca
@email: bmixonb1@cs.unm.edu

"""
import subprocess as sp
import random as rand
import copy
from multiprocessing import Pool
import numpy as np

LAST_IDX = -1
PEER = 0
WILKENS = 10

# Constants for indexing into the battle result tuple.
WARRIOR1_TUPLE = 0
WARRIOR2_TUPLE = 1

# Constants for indexing into a warrior tuple
WARRIOR_IDX = 0
WARRIOR_NAME = 1
BATTLE_SCORE = 2
WILKS_MULTIPLIER = 10
TOURNAMENT_SELECTION = 0
ROULETTE_SELECTION = 1
RANDOM_SELECTION = 2

SELECTION_STRS = [
    "TOURNAMENT_SELECTION",
    "ROULETTE_SELECTION",
    "RANDOM_SELECTION"
]

NUMBER_TOURNAMENTS = 4
NUMBER_POOL_WORKERS = 8

"""
The list of benchmark warriors. Note that some 
are included multiple times. This is to ensure that
warriors that we didn't do so well against are 
selected more often. The idea being that we 
can get better at fighting warriors in these
classes should they contain different types of 
instructions. We seem to be tuned towards warriors like
TIME, MARCIA, and NOBODY. That shouldn't be
surprising since the code is designed to build
more similarly to those warriors.
"""

BENCHMARK_WARRIORS = [
    'WilkiesBench/BLUEFUNK.RED',
    'WilkiesBench/CANNON.RED',
    'WilkiesBench/FSTORM.RED',
    'WilkiesBench/IRONGATE.RED',
    'WilkiesBench/MARCIA13.RED',
    'WilkiesBench/NOBODY.RED',
    'WilkiesBench/PAPERONE.RED',
    'WilkiesBench/PSWING.RED',
    'WilkiesBench/RAVE.RED',
    'WilkiesBench/THERMITE.RED',
    'WilkiesBench/TIME.RED',
    'WilkiesBench/TORNADO.RED'
]


def get_fitness(results, multiplier):
    warrior_score = int(results[0].split(" ")[LAST_IDX])
    return multiplier * warrior_score

def pair_wise_run(warrior):
    """pair_wise_run
    Used at the beginning of every selection algorithm. This function
    is mapped accross each of the 64 warriors to do a pair-wise comparision
    of all warriors with all Wilks robots in parallel using process
    pools from the mulitprocessing module.
    """
    for benchmark in BENCHMARK_WARRIORS:
        """ """
        program = ["./pmars", "-b", "-r", "101"]
        args = ['./tmp_warriors/' + str(warrior), benchmark]
        program += args
        results = sp.check_output(program)
        results = results.split("\n")
        warrior.fitness_multiplier += get_fitness(results, WILKS_MULTIPLIER)

class GAScheduler:
    """ 
    The GA Scheduler is a class designed to 
    take care of scheduling GA competators.
    """

    def __init__(self, **kwargs):
        """ 
        Required keyword arguments:
        ga: The GeneticAlgorithm object we are associated with.
        k:
        """
        self.process_pool = Pool(NUMBER_POOL_WORKERS)
        self.ga = kwargs["ga"]
        selection_type = self.ga.selection
        if selection_type == TOURNAMENT_SELECTION:
            self.k = kwargs["k"]
            self._selection = self._tournament_selection

        elif selection_type == ROULETTE_SELECTION:
            self._selection = self._roulette_selection

        else:
            self._selection = self._random_replacement_selection

    def _build_pmar_args(self, **kwargs):
        """_build_pmar_args:
        
        Build an argument list consisting of the warriors
        passed in via the keyword arguements.
        
        Return: 
        
        A list of strings containing the warriors to be run with pmars.
        """
        args = []
        warriors = kwargs["warriors"]
        for warrior in warriors:
            args += ["./tmp_warriors/" + str(warrior)]
        return args

    def _parse_results(self, **kwargs):
        """_parse_results
        
        Really gross function for parsing the output from a pmars run.
        
        """
        results = kwargs["results"]
        warrior1_idx = kwargs["warrior1_idx"]
        warrior2_idx = kwargs["warrior2_idx"]

        competition_type = kwargs["competition_type"]
        results = results.split("\n")

        w1results = results[0].split(" ")
        w2results = results[1].split(" ")
        warrior1name, warrior1_score = None, None

        if competition_type == PEER:
            warrior1name, warrior1_score = w1results[1], w1results[
                6].replace(":", "").replace("\n", "")
        else:
            lw1s = len(w1results) - 1
            warrior1name, warrior1_score = w1results[1], w1results[
                lw1s].replace("\n", "")

        lw2 = len(w2results)
        warrior2name, warrior2_score = w2results[1], w2results[lw2 - 1]
        if competition_type == PEER:  # We are testing against ourselves.
            return ((warrior1_idx, warrior1name, warrior1_score, competition_type),
                    (warrior2_idx, warrior2name, warrior2_score, competition_type))
        else:  # We are testing against Wilens benchmarks.
            return ((warrior1_idx, warrior1name, warrior1_score, competition_type),
                    (warrior2_idx, warrior2name, warrior2_score, competition_type))

    def _get_winner_loser_idxs(self, battle_tuple):
        """ 
        find the winner and loser from the battle tuple.
        """
        warrior1_tuple = battle_tuple[WARRIOR1_TUPLE]
        warrior2_tuple = battle_tuple[WARRIOR2_TUPLE]
        score1, score2 = int(warrior1_tuple[BATTLE_SCORE]), int(
            warrior2_tuple[BATTLE_SCORE])
        warrior1 = warrior1_tuple[WARRIOR_IDX]
        warrior1 = self.ga.warriors[warrior1]

        warrior2 = warrior2_tuple[WARRIOR_IDX]
        warrior2 = self.ga.warriors[warrior2]

        winner_idx, loser_idx = None, None
        if (score1 + warrior1.fitness_multiplier) > (score2 + warrior2.fitness_multiplier):
            winner_idx = warrior1_tuple[WARRIOR_IDX]
            loser_idx = warrior2_tuple[WARRIOR_IDX]
        else:
            winner_idx = warrior2_tuple[WARRIOR_IDX]
            loser_idx = warrior1_tuple[WARRIOR_IDX]
        return winner_idx, loser_idx
    
    def _set_winner_loser_fitness(self, battle_tuple):
        """ 
        Sets the raw score of the warrior after a battle. 

        We are lazy so if one of the warriors happened to have been selected twice,
        won 1 and lost 1 in that order, the loss would clobber the win.
        """
        warrior1_tuple = battle_tuple[WARRIOR1_TUPLE]
        warrior2_tuple = battle_tuple[WARRIOR2_TUPLE]
        score1, score2 = int(warrior1_tuple[BATTLE_SCORE]), int(
            warrior2_tuple[BATTLE_SCORE])
        warrior1_idx, warrior2_idx = warrior1_tuple[
            WARRIOR_IDX], warrior2_tuple[WARRIOR_IDX]
        warriors = self.ga.warriors
        warrior1, warrior2 = warriors[warrior1_idx], warriors[warrior2_idx]
        warrior1.set_fitness(score1)
        warrior2.set_fitness(score2)

    def _run_pmars(self, **kwargs):
        """_run_pmars
        helper function to just run two warriors against eachother
        in pmars.

        This function needs to set the score of the warriors

        """
        warrior1, warrior2 = kwargs["warrior1"], kwargs["warrior2"]
        warrior1_idx, warrior2_idx = kwargs[
            "warrior1_idx"], kwargs["warrior2_idx"]
        progam = ["./pmars", "-b", "-r", "101"]
        progam += self._build_pmar_args(warriors=[warrior1, warrior2])
        results = sp.check_output(progam)
        battle_tuple = self._parse_results(results=results,
                                           warrior1_idx=warrior1_idx,
                                           warrior2_idx=warrior2_idx,
                                           competition_type=kwargs["competition_type"])

        self._set_winner_loser_fitness(battle_tuple)
        winner, loser = self._get_winner_loser_idxs(battle_tuple)
        return winner, loser

    def _next_generation(self, next_generation_idxs):
        """_next_generation
        
        Required arguments:

        next_generation_idxs: A list of integers corresponding to the list of warriors.
        
        Generates a list of warriors for the next generation given
        the ``next_generation_idxs" argument
        for all i in
           1. Perform all the deep copying of warriors using the index list.
           2. append them to a list.
        return the next generation.
        """
        next_generation = []
        for i in next_generation_idxs:
            next_generation.append(copy.deepcopy(self.ga.warriors[i]))
        return next_generation

    def _roulette_next_generation_idxs(self):
        next_generation_idxs = []
        warriors = sorted(self.ga.warriors, key=lambda w: w.fitness)
        total_fitness = 0

        for warrior in warriors:
            total_fitness += warrior.fitness

        for _ in range(self.ga.population_size):
            value = rand.random() * total_fitness
            for i in range(self.ga.population_size - 1, -1, -1):
                value -= warriors[i].fitness
                if value <= 0:
                    next_generation_idxs.append(i)
                    break
        return next_generation_idxs

    def selection(self):
        """selection:
        
        Get an initial fitness score by performing pair-wise pmars runs 
        between all warriors and all Wilks warriors. This is parallelized
        using a process pool stored in the scheduler.

        """
        self.process_pool.map(pair_wise_run, self.ga.warriors)

        next_generation = self._selection()

        tmp = map(lambda w: w.fitness, self.ga.warriors)
        
        self.ga.min_fitness_over_time.append(min(tmp))
        self.ga.mean_fitness_over_time.append(np.mean(tmp))
        self.ga.max_fitness_over_time.append(max(tmp))
        top = sorted(next_generation)
        top_warrior = top[-1]
        # Do a final comparison so we can see how a warrior improves over time.
        # This stuff can probably be removed when we are ready to have matthew pull
        # The code and run our GA.
        old_fitness = top_warrior.fitness
        old_multipler = top_warrior.fitness_multiplier
        top_warrior.fitness_multiplier = 0
        top_warrior.fitness = 0

        for i in range(5):
            pair_wise_run(top_warrior)
        self.ga.top_fitness_over_time.append((top_warrior.fitness_multiplier/5.0))
        self.ga.top_warriors.append(copy.deepcopy(top_warrior))
        top_warrior.fitness = old_fitness
        top_warrior.fitness_multiplier = old_multipler 
        return next_generation

    def _roulette_selection(self):
        """_roulette_selection

        Implements the roulette selection algorithm

        Details:

        Source [2] https://en.wikipedia.org/wiki/Fitness_proportionate_selection
        """
        rand.shuffle(self.ga.warriors)
        warriors = self.ga.warriors
        for i in range(0, self.ga.population_size, 2):
            warrior1_idx, warrior2_idx = i, i + 1
            _, _ = self._run_pmars(
                warrior1=str(warriors[warrior1_idx]),
                warrior1_idx=warrior1_idx,
                warrior2=str(warriors[warrior2_idx]),
                warrior2_idx=warrior2_idx,
                competition_type=PEER
            )
        next_generation_idxs = self._roulette_next_generation_idxs()
        return self._next_generation(next_generation_idxs)

    def _select_tournament_warriors(self):
        """ 
        Pick 4 unique indeces from the population as warriors for the 
        4 warrior tournament.
        """
        warriors_idxs = []
        while len(warriors_idxs) < 4:
            warrior = rand.randint(0, self.ga.population_size - 1)
            if warrior not in warriors_idxs:
                warriors_idxs += [warrior]
        return warriors_idxs

    def _run_tournament(self):
        """_run_tournament
        Run 1 tournament. 

        64 == population size, 4 = tournament size, 16 = number of tournaments held

        64/4 = 16 => 4 tournaments total to generate the next intermediate population.

        Therefore, this program should:

        1.   Randomly select 4 individuals
        1.2. Battle these 4 to compute a fitness score for each. There are now two left. 
        1.3. Battle the two in the first heat. The winner is added to the candidate list.

        2. repeat 15 more times. 

        """

        candidate_idxs = []

        warriors = self.ga.warriors
        popsize = self.ga.population_size

        for i in range(16):  # WARNING HARDCODED VALUE, GROSS!!! should change
            warrior1_idx, warrior2_idx, warrior3_idx, warrior4_idx = \
                self._select_tournament_warriors()

            winner1, losser1 = self._run_pmars(
                warrior1=str(warriors[warrior1_idx]),
                warrior1_idx=warrior1_idx,
                warrior2=str(warriors[warrior2_idx]),
                warrior2_idx=warrior2_idx,
                competition_type=PEER
            )

            winner2, losser2 = self._run_pmars(
                warrior1=str(warriors[warrior3_idx]),
                warrior1_idx=warrior3_idx,
                warrior2=str(warriors[warrior4_idx]),
                warrior2_idx=warrior4_idx,
                competition_type=PEER
            )

            winner, losser = self._run_pmars(
                warrior1=str(warriors[winner1]),
                warrior1_idx=winner1,
                warrior2=str(warriors[winner2]),
                warrior2_idx=winner2,
                competition_type=PEER
            )
            candidate_idxs += [winner]
        return candidate_idxs

    def _tournament_selection(self):
        """_tournament_selection

        Implements the tournament selection algorithm.

        Details:

        "A comparison of selection schemes used in genetic algoritms",
        Tobias Blickle and Lothar Thiele

        Chapter 3: Tournament Selection

        """
        next_generation_idxs = []
        for match in range(NUMBER_TOURNAMENTS):
            candidate_idxs = self._run_tournament()
            next_generation_idxs += candidate_idxs
        return self._next_generation(next_generation_idxs)

    def _random_replacement_selection(self):
        """ random_replacement_schedule:

        This scheduler should implement the random replacement of the 
        replacement of the bottom half of the population 
        with the top half and return the result as a the 
        next generation.

        Shuffle into a random order before walking along by twos.

        """
        rand.shuffle(self.ga.warriors)
        warriors = self.ga.warriors
        for i in range(0, self.ga.population_size, 2):
            warriors[i].idx = i
            warriors[i + 1].idx = i + 1
            warrior1_idx, warrior2_idx = i, i + 1
            _, _ = self._run_pmars(
                warrior1=str(warriors[warrior1_idx]),
                warrior1_idx=warrior1_idx,
                warrior2=str(warriors[warrior2_idx]),
                warrior2_idx=warrior2_idx,
                competition_type=PEER
            )
        next_generation_idxs = []
        i = 0
        for warrior in sorted(warriors, key=lambda w: w.fitness):
            """ """
            if i > 31:
                next_generation_idxs.append(warrior.idx)
                next_generation_idxs.append(warrior.idx)
            i += 1
        return self._next_generation(next_generation_idxs)
