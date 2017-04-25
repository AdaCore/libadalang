from __future__ import absolute_import, division, print_function

import Queue
from threading import Thread


def pmap(fn, collection, nb_threads=4):
    """
    Apply fn to each item in collection, returns a collection containing the
    results. Uses threads to do the processing.

    NOTES:
    - Since python uses a GIL, you won't get real parallelism except if your
      underlying function spawns processes (which is our case in the test
      driver).
    - Order is not preserved by that algorithm, so it is not strictly a
      parallel map.
    - Items from collection are all consumed at once at the beginning to
      simplify the algorithm.
    """

    # Create an in queue and an out queue. The in queue will contain all the
    # data to be processed by the worker threads. The out queue will contain
    # the results.
    in_queue, out_queue = Queue.Queue(), Queue.Queue()

    # Fill in the in queue with the items to process
    map(in_queue.put, collection)

    def consume():
        """
        Consumer function to be executed by all threads. Consume one item,
        process it with fn, and put it in the out queue. Stop the thread when
        the queue is empty.
        """
        while True:
            try:
                out_queue.put(fn(in_queue.get_nowait()))
            except Queue.Empty:
                return

    # Create the worker threads and start them
    threads = [Thread(target=consume) for _ in range(nb_threads)]
    map(Thread.start, threads)

    while True:
        # If there is no item left in the in queue, wait for the threads to end
        # and return the elements remaining in the out queue.
        if in_queue.empty():
            map(Thread.join, threads)
            while not out_queue.empty():
                yield out_queue.get()
            return
        # If the in queue still contains elements, just return the results as
        # they come.
        else:
            yield out_queue.get()
