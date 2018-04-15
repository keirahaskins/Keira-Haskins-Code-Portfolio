#!/usr/bin/env python

"""
@group: 
@author: Keira Haskins
@email:
@author: Ben Mixon-Baca
@email: bmixonb1@cs.unm.edu

"""

import warrior as w
import sys
import random as rand
import time
import subprocess as sp
from multiprocessing import Pool
import numpy as np
import gascheduler as sched
import numpy as np
# Uncomment the lines below if there is an error
# import matplotlib
# matplotlib.use('Agg')
import matplotlib.pyplot as plt  # For matlab style plots
from scipy.stats import *  # For ranksums (Mann - Whitney test)


max_population = 64

NUMBER_GENERATIONS = 20

genetic_algorithm_configurations = [None, None, None, None, None, None,
                                    None, None, None, None, None, None,
                                    None, None, None, None, None, None]
NO_CROSSOVER        = 0
UNIFORM_CROSSOVER   = 1
ONE_POINT_CROSSOVER = 2

CROSSOVER_STRS = [
    "no crossover",
    "uniform crossover",
    "one-point crossover"
]

TOURNAMENT_K = 4


class GeneticAlgorithm:
    """ 
    GeneticAlgorithm implements the funcionallity of a genetic algorithm and 
    transparently takes care of the functionality of running competators.
    """

    def __init__(self, **kwargs):
        """ Create a GeneticAlgorithm object

        Required keyword arguements:

        population_size: The size of the initial population

        crossover: one of

        geneticalgorithm.NO_CROSSOVER
        geneticalgorithm.UNIFORM_CROSSOVER
        geneticalgorithm.ONE_POINT_CROSSOVER

        crossover_rate: A float in [0,1). The percentage of time we perform crossover.

        selection: One of 

        gascheduler.TOURNAMENT_SECELCTION
        gascheduler.ROULETTE_SELECTION
        gascheduler.RANDOM_LOWER

        elitism: A boolean defining where to use elitism or not.

        number_generations: An integer. The number of generations


        """

        self.selection = kwargs["selection"]
        self.scheduler = sched.GAScheduler(ga=self,
                                           k=TOURNAMENT_K)
        self.min_fitness_over_time = []
        self.mean_fitness_over_time = []
        self.max_fitness_over_time = []

        self.population_size = kwargs["population_size"]
        self.number_generations = kwargs["number_generations"]
        self.elitism = kwargs["elitism"]
        self.elite = None
        self.warriors = self._generate_warriors(
            nwarriors=kwargs["population_size"],
            mutation_rate=kwargs["mutation_rate"])
        self.uniform_crossover_mixing_ratio = kwargs[
            "uniform_crossover_mixing_ratio"]
        self.crossover_rate = kwargs["crossover_rate"]
        self.crossover_technique = kwargs["crossover"]
        if self.crossover_technique == UNIFORM_CROSSOVER:
            self._crossover = self._uniform_crossover
        elif self.crossover_technique == ONE_POINT_CROSSOVER:
            self._crossover = self._single_point_crossover
        configuration = sched.SELECTION_STRS[self.selection] + ','
        configuration += CROSSOVER_STRS[self.crossover_technique] + ','
        if self.elitism:
            configuration += "elitism,"
        else:
            configuration += "no elitism,"
        configuration += 'number_generations=' + \
            str(self.number_generations) + ','
        configuration += 'crossover_rate=' + str(self.crossover_rate) + ','
        configuration += 'mutation_rate=' + str(kwargs["mutation_rate"])
        self.configuration = configuration
        self.first_place  = None
        self.second_place = None
        self.top_fitness_over_time = []
        self.top_warriors = []

    def write_statistics_to_file(self):

        with open('./statistics/' + self.configuration + '.txt', "w") as stat_file:
            stat_file.write('configuration:                    ' +
                            self.configuration + '\n')
            stat_file.write('first place:                      ' +
                            str(self.first_place) + '\n')
            stat_file.write('second place:                     ' +
                            str(self.second_place) + '\n')
            stat_file.write('min_fitness_over_time:            ' +
                            str(self.min_fitness_over_time) + '\n')
            stat_file.write('mean_fitness_over_time:           ' +
                            str(self.mean_fitness_over_time) + '\n')
            stat_file.write('max_fitness_over_time:              ' +
                            str(self.max_fitness_over_time) + '\n')
            stat_file.write('top warrior wins vs wilks over time:' +
                            str(self.top_fitness_over_time) + '\n')

    def crossover(self, **kwargs):
        """crossover: The actual function called in the loop of runGA.

        Default: Do nothing.

        If a crossover parameter is passed to the GeneticAlgorithm class, then this 
        function is overriden.
        """
        next_generatation = kwargs["warriors"]
        if self.crossover_technique == NO_CROSSOVER:
            return next_generatation
        else:
            """ """
            children = []
            while len(next_generatation) > 0 or len(next_generatation) > 0:
                parent1 = rand.choice(next_generatation)
                next_generatation.remove(parent1)
                parent2 = rand.choice(next_generatation)
                next_generatation.remove(parent2)
                child1, child2 = self._crossover(
                    warrior1=parent1, warrior2=parent2)
                children.append(child1)
                children.append(child2)
            if len(next_generatation) == 1:
                w = next_generatation[0]
                next_generatation.remove(w)
                children.append(w)
            return children

    def _generate_warriors(self, **kwargs):
        """generate_warriors generates the set of redcode warriors
        """
        nwarriors = kwargs["nwarriors"]
        warriors = []
        for i in range(nwarriors):
            warrior = w.Warrior(mutation_rate=kwargs["mutation_rate"])
            warriors.append(warrior)
        return warriors

    def _crossover(self, **kwargs):
        """ """
        pass

    def _single_point_crossover(self, **kwargs):
        """
        Required keyword arguments:

        warrior1: A warrior to evolve
        warrior2: A warrior to evolve.

        Choose random row and swap all columns. Addresses corner case.
p        """
        warrior1 = kwargs["warrior1"]
        warrior2 = kwargs["warrior2"]
        child1, child2 = None, None
        if rand.random() < self.crossover_rate:
            rand_row = rand.randint(
                0, w.GENOME_ROWS - 1)

            # Double check that this is correct.
            for row in range(rand_row, w.GENOME_ROWS):
                for col in range(0, w.GENOME_COLS):
                    tmp = warrior2.genome[row][col]
                    warrior2.genome[row][col] = warrior1.genome[row][col]
                    warrior1.genome[row][col] = tmp
            child1, child2 = warrior1, warrior2

        else:
            child1, child2 = warrior1, warrior2

        return child1, child2

    def _uniform_crossover(self, **kwargs):
        """
        Required keyword arguments:

        warrior1: A warrior to evolve.
        warrior2: A warrior to evolve.

        Choose random row and swap all columns. Addresses corner case.
        """
        warrior1 = kwargs["warrior1"]
        warrior2 = kwargs["warrior2"]

        child1, child2 = None, None
        if rand.random() < self.crossover_rate:
            """ """
            for row in range(w.GENOME_ROWS):
                if rand.random() < self.uniform_crossover_mixing_ratio:
                    for col in range(w.GENOME_COLS):
                        tmp = warrior2.genome[row][col]
                        warrior2.genome[row][col] = warrior1.genome[row][col]
                        warrior1.genome[row][col] = tmp
            child1, child2 = warrior1, warrior2
        else:
            child1, child2 = warrior1, warrior2
        return child1, child2

    def runGA(self, **kwargs):

        current_generation = 0
        while current_generation < self.number_generations:
            """ 
            # http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.11.509&rep=rep1&type=pdf
            # "it is assumed the selection and recombination are done sequentially."
            # 1. Choose a population

            It is assumed that selection and recombination are done sequentially: 
            First a selection phase creates an intermediate population

            """
            next_generation = self.scheduler.selection()
            if self.elitism:
                self.elite = sorted(
                    next_generation, key=lambda w: w.fitness)[-1]
            next_generation = self.crossover(warriors=next_generation)

            self.scheduler.process_pool.map(
                mutate_warrior, next_generation)  # Parallelize mutation

            if self.elitism:
                next_generation = sorted(
                    next_generation, key=lambda w: w.fitness)
                next_generation.remove(next_generation[0])
                next_generation.append(self.elite)
                self.elite = None

            # Parallelize name updates and redcode generation.
            self.scheduler.process_pool.map(
                update_name_redcode, next_generation)
            self.warriors = next_generation
            current_generation += 1

        # Do a final round of tests against the benchmarks to get final fitness scores,
        # Sort the warriors
        final_warriors = sorted(self.top_warriors, key=lambda w: w.fitness_multiplier)
        # final_warriors = sorted(self.warriors, key=lambda w: w.fitness)
        final_warrior1, final_warrior2 = final_warriors[-1], final_warriors[-2]

        final_warrior1.name = "T3-1st-place-warrior.RED"
        final_warrior1.print_redcode_to_file(
            "./final_warriors/")
        self.first_place = final_warrior1
 
        final_warrior2.name = "T3-2nd-place-warrior.RED"
        final_warrior2.print_redcode_to_file(
            "./final_warriors/")
        self.second_place = final_warrior2


def mutate_warrior(warrior):
    warrior.mutate()


def update_name_redcode(warrior):
    warrior.new_name()
    warrior.print_redcode_to_file("./tmp_warriors/")


def configure_ga(population_size, number_generations, selection, crossover,
                 crossover_rate, uniform_crossover_mixing_ratio, mutation_rate, elitism):
    """configure_ga takes parameters necassary for configuring the GeneticAlgorithm 
    class (GA), configures it, and returns it.

    Required parameters:

    population_size, 
    number_generations, 
    selection, 
    crossover: The crossover technique to use. 

    Domain: gascheduler.[TOURNAMENT_SELECTION | ROULETTE_SELECTION | RANDOM_SELECTION]

    crossover_rate: The rate at which crossover occurs. Domain [0, 1]. 
                    All numbers are acceptable but only values in the unit interval
                    make sense.

    uniform_crossover_mixing_ratio: 

    When uniform-crossover is used, this parameter specifies how much crossover should occur 
    between parents. Domain [0, 1]. 

    All numbers are acceptable but only values in the unit interval
    make sense.

    mutation_rate: The probability that a given warrior will mutate. Domain [0, 1]. 
                   All numbers are acceptable but only values in the unit interval
                   make sense.

    elitism: whether to use elitism. Domain [True | False]
    """
    ga = GeneticAlgorithm(population_size=population_size,
                          number_generations=number_generations,
                          selection=selection,
                          crossover=crossover,
                          crossover_rate=crossover_rate,
                          mutation_rate=mutation_rate,
                          uniform_crossover_mixing_ratio=uniform_crossover_mixing_ratio,
                          elitism=elitism)
    return ga


def make_generation_plot(x, y, title, fname):
    f = plt.figure()
    ax = plt.axes()
    ax.set_title(title)
    plt.plot(x, y)
    f.savefig("./plots/" + fname, bbox_inches='tight')


def make_box_plot(data, title, fname, xTickLabels):
    """make_box_plot 
    """
    f = plt.figure()
    ax = plt.axes()
    ax.set_title(title)
    xtickNames = plt.setp(ax, xticklabels=xTickLabels)
    plt.setp(xtickNames, rotation=45, fontsize=8)

    plt.boxplot(data, notch=False, sym='+', vert=True, whis=1.5, positions=None,
                widths=None, patch_artist=False, bootstrap=None, usermedians=None,
                conf_intervals=None)
    f.savefig("./plots/" + fname, bbox_inches='tight')


def section_three_two():
    """section_three_two: 

    Implements the analyses of section 3.2.

    3.2. Comparison of Selection Methods

    3.2.1. Generate a figure that compare the rate of convergence for your GA using
    roulette, tournament, and by random replacement of the lowest fitness half
    of the population with members of the upper half.

    3.2.2.  Generate an interquartile box-plot over many samples showing the quality
    of your solutions for these 4 types of selection.  Provide p-values using the
    Mann-Whitney (also called rank-sum) statistical test to show whether the
    distribution of solution qualities is statistically different from one another.

    3.2.3.  Report whether elitism improves performance for each of the 4 selection
    methods.  Verify your claim using the Mann-Whitney test.
    """
    population_size = 64
    number_generations = 50

    selection_list = [sched.TOURNAMENT_SELECTION,
                      sched.ROULETTE_SELECTION, sched.RANDOM_SELECTION]
    crossover = UNIFORM_CROSSOVER
    crossover_rate = 0.7
    mutation_rate = 0.01
    uniform_crossover_mixing_ratio = 0.5  # Half of each parent
    elitism_list = [True, False]

    print(selection_list)
    for elitism in elitism_list:
        ga = None
        selection_mean = []
        labels = []
        for selection in selection_list:
            ga = configure_ga(population_size,
                              number_generations,
                              selection,
                              crossover,
                              crossover_rate,
                              uniform_crossover_mixing_ratio,
                              mutation_rate,
                              elitism)
            ga.runGA()
            ga.write_statistics_to_file()
            selection_mean.append(ga.mean_fitness_over_time)
            labels.append(sched.SELECTION_STRS[selection])

            x = range(1, len(ga.mean_fitness_over_time) + 1)
            y = ga.mean_fitness_over_time
            y1 = ga.top_fitness_over_time

            if elitism:
                make_generation_plot(x, y, sched.SELECTION_STRS[selection] + ": With Elitism",
                                     sched.SELECTION_STRS[selection] + "-with-elitism.pdf")
                make_generation_plot(x, y1,  'Top Warrior fitness over time'+sched.SELECTION_STRS[selection] + ": With Elitism",
                                     'top-warrior-'+sched.SELECTION_STRS[selection] + "-with-elitism.pdf")
            else:
                make_generation_plot(x, y, sched.SELECTION_STRS[selection] + ": With Elitism",
                                     sched.SELECTION_STRS[selection] + "-no-elitism.pdf")
                make_generation_plot(x, y1, 'Top Warrior fitness over time: ' + sched.SELECTION_STRS[selection] + ": With Elitism",
                                     'top-warrior-' + sched.SELECTION_STRS[selection] + "-no-elitism.pdf")
        if elitism:
            make_box_plot(selection_mean, "Box plot of mean fitness: With Elitism",
                          "elitism-selection-boxplot.pdf", labels)
        else:
            make_box_plot(selection_mean, "Box plot of mean fitness: No Elitism",
                          "no-elitism-selection-boxplot.pdf", labels)


def section_three_three():
    """section_three_three: 

    Implements the analyses of section 3.3.

    3.3. Comparison of Crossover Method

    3.3.1. Generate a figure that compares the rate of convergence for your GA using
    no crossover, uniform crossover, and 1-point crossover.

    3.3.2.  Generate an interquartile box-plot over many samples showing the quality
    of your solutions for these 3 types of crossover.  Provide p-values using the
    Mann-Whitney (also called rank-sum) statistical test to show whether the
    distribution of solution qualities is statistically different from one another.
    """
    population_size = 64
    number_generations = 50
    selection = sched.ROULETTE_SELECTION
    crossover_rate = 0.07
    mutation_rate = 0.01
    uniform_crossover_mixing_ratio = 0.5
    elitism = True
    crossover_list = [NO_CROSSOVER, UNIFORM_CROSSOVER, ONE_POINT_CROSSOVER]
    crossover_results = []
    crossover_labels = []

    for crossover in crossover_list:
        ga = configure_ga(population_size,
                          number_generations,
                          selection,
                          crossover,
                          crossover_rate,
                          uniform_crossover_mixing_ratio,
                          mutation_rate,
                          elitism)
        ga.runGA()
        ga.write_statistics_to_file()

        x = range(1, len(ga.mean_fitness_over_time) + 1)
        y1 = ga.top_fitness_over_time

        y = ga.mean_fitness_over_time
        crossover_results.append(y)
        crossover_labels.append(CROSSOVER_STRS[crossover])
        make_generation_plot(x, y, "Mean fitness over time" + CROSSOVER_STRS[crossover] + "",
                             CROSSOVER_STRS[crossover] + ".pdf")
        make_generation_plot(x, y1, 'Top warrior fitness over time-'+CROSSOVER_STRS[crossover] + "",
                             'top-warrior-'+CROSSOVER_STRS[crossover] + ".pdf")

    make_box_plot(crossover_results, "Crossover method box plot: mean fitness ",
                  "crossover-boxplot.pdf", crossover_labels)


def section_three_four():
    """section_three_four

    Implements section 3.4 analyses.

    3.4. Define a mutation operator.  Produce figures showing the effects of changing the
    mutation rate and crossover rate on your genetic algorithm.
    """
    population_size = 64
    number_generations = 50
    selection = sched.ROULETTE_SELECTION
    crossover_rate = 0.7
    uniform_crossover_mixing_ratio = 0.5  # Half of each parent
    elitism = True
    crossover = UNIFORM_CROSSOVER
    mutation_rate_list = [0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008, 0.009, 0.01,
                          0.012, 0.014, 0.016, 0.018, 0.02, 0.025, 0.03, 0.035, 0.04]

    mutation_rate_results = []
    for mutation_rate in mutation_rate_list:
        ga = configure_ga(population_size,
                          number_generations,
                          selection,
                          crossover,
                          crossover_rate,
                          uniform_crossover_mixing_ratio,
                          mutation_rate,
                          elitism)
        ga.runGA()
        ga.write_statistics_to_file()
        mutation_rate_results.append(ga.mean_fitness_over_time[-1])
        y1 = ga.top_fitness_over_time
        make_generation_plot(range(1, len(ga.top_fitness_over_time) + 1), y1, 'top-warrior-mutation-rate-' + str(mutation_rate),
                             'top-warrior-mutation-rate-' + str(mutation_rate) + "-with-elitism.pdf")
        make_generation_plot(range(1, len(ga.top_fitness_over_time) + 1), ga.mean_fitness_over_time, 'Mean fitness over time-mutation-rate-' + str(mutation_rate),
                             'mean-fitness-over-time-mutation-rate-' + str(mutation_rate) + "-with-elitism.pdf")

    make_generation_plot(range(1, len(mutation_rate_results) + 1), mutation_rate_results, 'Final mean fitness over time, Mutation Rate ',
                         'top-warrior-mutation-rate-with-elitism.pdf')

    crossover_rate_list = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9]
    crossover_rate_results = []
    mutation_rate = 0.005
    for crossover_rate in crossover_rate_list:
        ga = configure_ga(population_size,
                          number_generations,
                          selection,
                          crossover,
                          crossover_rate,
                          uniform_crossover_mixing_ratio,
                          mutation_rate,
                          elitism)
        ga.runGA()
        ga.write_statistics_to_file()
        crossover_rate_results.append(ga.mean_fitness_over_time[-1])
        y1 = ga.top_fitness_over_time
        make_generation_plot(range(1, len(ga.top_fitness_over_time) + 1), y1, 'top-warrior fitness-crossover-rate-' + str(crossover_rate),
                             'top-crossover-rate-' + str(crossover_rate) + ".pdf")
        make_generation_plot(range(1, len(ga.top_fitness_over_time) + 1), ga.mean_fitness_over_time, 'Mean fitness over time-crossover-rate-' + str(crossover_rate),
                             'mean-fitness-over-time-crossover-rate-' + str(crossover_rate) + "-with-elitism.pdf")
    make_generation_plot(range(1, len(crossover_rate_results) + 1), crossover_rate_results, 'Final mean fitness over time, Crossover Rate ',
                         'mean-fitness-over-time-crossover-rate-with-elitism.pdf')


def fullyrandomized_fullfactorial():
    """fullyrandomized_fullfactorial

    HERE BE DRAGONS!!!

    For edification, perform a fully randomized, full-factorial experiment of all
    parameters possible for the genetic algorithm. 

    WARNING: THIS WILL TAKE LIKELY SEVERAL DAYS TO FINISH.

    3 * 2 * 3 * 8 * 5 * 10 = 7200.... Yea, this could take a while...

    """
    population_size = 64
    number_generations = 50

    selection_list = [sched.TOURNAMENT_SELECTION,
                      sched.ROULETTE_SELECTION, sched.RANDOM_SELECTION]
    rand.shuffle(selection_list)

    elitism_list = [True, False]
    rand.shuffle(elitism_list)

    crossover_list = [NO_CROSSOVER, UNIFORM_CROSSOVER, ONE_POINT_CROSSOVER]
    rand.shuffle(crossover_list)

    crossover_rate_list = [i * 0.1 for i in range(1, 10)]
    rand.shuffle(crossover_rate_list)

    uniform_crossover_mixing_ratio_list = [i * 0.1 for i in range(3, 8)]
    rand.shuffle(uniform_crossover_mixing_ratio_list)

    mutation_rate_list = [i * 0.1 for i in range(0, 11)]
    rand.shuffle(mutation_rate_list)

    for uniform_crossover_mixing_ratio in uniform_crossover_mixing_ratio_list:
        for selection in selection_list:
            for crossover in crossover_list:
                for crossover_rate in crossover_rate_list:
                    for elitism in elitism_list:
                        for mutation_rate in mutation_rate_list:
                            ga = configure_ga(population_size, number_generations, selection,
                                              crossover, crossover_rate,
                                              uniform_crossover_mixing_ratio, mutation_rate,
                                              elitism)
                            ga.runGA()
                            ga.write_statistics_to_file()



def island_ga(island_num):
    """ """
    population_size = 64
    number_generations = 50
    selection =  sched.ROULETTE_SELECTION
    crossover = ONE_POINT_CROSSOVER
    crossover_rate = 0.2
    mutation_rate = 0.01
    uniform_crossover_mixing_ratio = 0.5  # Half of each parent
    elitism = True
    ga = configure_ga(population_size,
                      number_generations,
                      selection,
                      crossover,
                      crossover_rate,
                      uniform_crossover_mixing_ratio,
                      mutation_rate,
                      elitism)
    ga.runGA()
    ga.write_statistics_to_file()

    x = range(1, len(ga.mean_fitness_over_time) + 1)
    y = ga.mean_fitness_over_time
    y1 = ga.top_fitness_over_time

    make_generation_plot(x, y, "Island ga mean fitness " + str(island_num),
                         "mean-fitness-island-ga-num"+ str(island_num)+ ".pdf")

    make_generation_plot(x, y1,  'Top Warrior fitness over time',
                         "top-warrior-island-ga-num"+ str(island_num)+ ".pdf")


def debugging_ga():
    """debugging_ga

    Run a naive GA for testing different configurations manually.

    The configuration below produced good results, graph ./plots/crossover/CrossoverVsTime5.pdf

    crossover_rate = 0.2,
    mutation_rate = 0.001,
    uniform_crossover_mixing_ratio = 0.2
    ga = configure_ga(max_population,  # population size
                      NUMBER_GENERATIONS,  # number of generations
                      sched.TOURNAMENT_SELECTION,  # selection technique
                      UNIFORM_CROSSOVER,  # crossover technique
                      crossover_rate,
                      uniform_crossover_mixing_ratio,  # %70 crossover rate
                      mutation_rate,
                      elitism=True)  # whether elitism is active or not.



    """
    crossover_rate = 0.1,
    mutation_rate = 0.001,
    uniform_crossover_mixing_ratio = 0.2
    ga = configure_ga(max_population,  # population size
                      NUMBER_GENERATIONS,  # number of generations
                      sched.RANDOM_SELECTION,  # selection technique
                      UNIFORM_CROSSOVER,  # crossover technique
                      crossover_rate,
                      uniform_crossover_mixing_ratio,  # %70 crossover rate
                      mutation_rate,
                      elitism=True)  # whether elitism is active or not.
    ga.runGA()
    ga.write_statistics_to_file()

    make_generation_plot(range(1, len(ga.top_fitness_over_time) + 1), ga.top_fitness_over_time, "Crossover vs time",
                         "CrossoverVsTime" + ".pdf")



def train_competition_candidate_warriors():
    """train_competition_candidates

    Trains the candidate warriors we will be submitting. Their assembly
    code can be found in the subdirectoy "final_warriors/*.RED". There
    will be two warrior redcode assembly language programs. 

    They are in ascii format as required by the "pmars" program. 

    """
    population_size = 64
    number_generations = 300 # Train for a lot longer since we have
    selection =  sched.ROULETTE_SELECTION
    crossover = ONE_POINT_CROSSOVER
    crossover_rate = 0.2
    mutation_rate = 0.01
    uniform_crossover_mixing_ratio = 0.5  # Half of each parent
    elitism = True
    ga = configure_ga(population_size,
                      number_generations,
                      selection,
                      crossover,
                      crossover_rate,
                      uniform_crossover_mixing_ratio,
                      mutation_rate,
                      elitism)

    ga.runGA() # Run the GA

    # ga.write_statistics_to_file()
    # make_generation_plot(range(1, len(ga.top_fitness_over_time) + 1), ga.top_fitness_over_time, "Training: Top Warrior Fitness vs Time",
    #                      "competition-top-warrior-fitness-vs-time.pdf")


def island_ga(island_num):
    """ """
    population_size = 64
    number_generations = 50 # Train for a lot longer since we have
    selection =  sched.ROULETTE_SELECTION
    crossover = ONE_POINT_CROSSOVER
    crossover_rate = 0.2
    mutation_rate = 0.01
    uniform_crossover_mixing_ratio = 0.5  # Half of each parent
    elitism = True
    ga = configure_ga(population_size,
                      number_generations,
                      selection,
                      crossover,
                      crossover_rate,
                      uniform_crossover_mixing_ratio,
                      mutation_rate,
                      elitism)
    # ga.runGA()
    # ga.write_statistics_to_file()
    # make_generation_plot(range(1, len(ga.top_fitness_over_time) + 1), ga.top_fitness_over_time, "Training: Top Warrior Fitness vs Time",
    #                    "competition-top-warrior-fitness-vs-time.pdf")

if __name__ == '__main__':
    """

    TODOs

    Project Analyses:

    1. Island GA's based on best performances
    """
    # debugging_ga()
    train_competition_candidate_warriors()
    # island_ga(0)
    # for island_num in range():
    #     island_ga(island_num)
