from numpy import random as rnd
import json


#state identifiers
state_free = 0
state_busy = 1
state_locked = 2

#statistic data
out_requests = list()
average_time_in_system = [0.0, 0]
average_interval = [0.0, 0]


class Request(object):
    def __init__(self, in_time):
        self.in_time = in_time
        self.out_time = -1.0


class Channel(object):
    def __init__(self, gen):
        self.gen = gen

        self.state = state_free
        self.request = None
        self.end_time = 0.0

        self.statistic = [0, 0, 0]

    def process_request(self, request, in_time):
        self.request = request
        self.state = state_busy
        self.end_time = in_time + next(self.gen)

    def release_request(self):
        self.request.out_time = self.end_time
        self.state = state_free

    def lock(self):
        self.state = state_locked

    def count_statistic(self):
        self.statistic[self.state] += 1


class Store(object):
    def __init__(self, max_size):
        self.max_size = max_size
        self.requests = list()
        self.statistic = [0, 0]
        self.last_used = 0.0

    def count_statistic(self):
        self.statistic = [self.statistic[0] + len(self.requests), self.statistic[1] + 1]


class Phase(object):
    def __init__(self, channels, store):
        self.channels = channels
        self.store = store


def normal_d(mu, sigma):
    while True:
        yield rnd.normal(mu, sigma)


def uniform_d(low, high):
    while True:
        yield rnd.uniform(low, high)


def exponential_d(lmbda):
    while True:
        yield rnd.exponential(lmbda)


if __name__ == '__main__':
    #system initial structure
    with open('input.json') as f_r:
        initial_structure = json.load(f_r)
    channel_counts = initial_structure[0]
    store_sizes = initial_structure[0]
    channel_distributions = [exponential_d(2.5), uniform_d(3.0, 9.0), exponential_d(2.0)]
    
    channels = [[Channel(channel_distributions[i]) for j in range(channel_counts[i])] for i in range(len(channel_counts))]
    stores = [Store(store_sizes[i]) for i in range(len(channel_counts))]
    phases = [Phase(channels[i], stores[i]) for i in range(len(channel_counts))]

    # modeling initial variables
    requests_count, current_count = 1000, 0
    rejected_requests, resolved_requests = list(), list()
    request_gen = exponential_d(1.0)
    system_time = 0.0

    request_interval = 0.0
    request_time = 0.0
    n = len(phases) - 1

    fp = open("stat_logger.txt", "w", encoding="utf-8")
    now_count = requests_count - current_count
    while current_count < requests_count:
        # check if we already can free some channels in last phase
        for channel in phases[n].channels:
            if channel.end_time <= system_time and channel.state == state_busy:
                channel.release_request()
                out_requests.append(channel.request)
                average_time_in_system[0] += (channel.request.out_time - channel.request.in_time)
                average_time_in_system[1] += 1
                average_interval[0] += out_requests[len(out_requests) - 1].out_time
                if len(out_requests) > 1:
                    average_interval[0] -= out_requests[len(out_requests) - 2].out_time

                average_interval[1] += 1

        # if we have some requests in store then process them
        while any(channel.state == state_free for channel in phases[n].channels):
            if len(phases[n].store.requests) > 0:
                request = phases[n].store.requests[0]
                phases[n].store.requests.pop(0)
                for channel in phases[n].channels:
                    if channel.state == state_free:
                        phases[n].store.last_used = max(phases[i].store.last_used, channel.end_time)
                        channel.process_request(request, channel.end_time)
                        break
            else:
                break

        # then lets go from last to first and try to send requests to next phase
        for i in reversed(range(len(phases) - 1)):
            can_go_next = any(channel.state == state_free for channel in phases[i + 1].channels)
            can_go_next &= (i + 1 != n) and len(phases[i + 1].store.requests) < phases[i + 1].store.max_size

            for channel in phases[i].channels:
                if channel.state == state_free:
                    continue

                if channel.state == state_busy and channel.end_time <= system_time:
                    if can_go_next:
                        request = channel.request
                        was = False
                        for next_channel in phases[i + 1].channels:
                            if next_channel.state == state_free:
                                channel.release_request()
                                next_channel.process_request(request, channel.end_time)
                                was = True
                                break

                        if not was:
                            channel.release_request()
                            phases[i + 1].store.requests.append(request)

                    else:
                        channel.lock()

                elif channel.state == state_locked and can_go_next:
                    request = channel.request
                    was = False
                    for next_channel in phases[i + 1].channels:
                        if next_channel.state == state_free:
                            channel.end_time = next_channel.end_time
                            channel.release_request()
                            next_channel.process_request(request, next_channel.end_time)
                            was = True
                            break

                    if not was:
                        channel.end_time = phases[i + 1].store.last_used
                        channel.release_request()
                        phases[i + 1].store.requests.append(request)

            while any(channel.state == state_free for channel in phases[i].channels):
                if len(phases[i].store.requests) > 0:
                    request = phases[i].store.requests[0]
                    phases[i].store.requests.pop(0)
                    for channel in phases[i].channels:
                        if channel.state == state_free:
                            phases[i].store.last_used = max(phases[i].store.last_used, channel.end_time)
                            channel.process_request(request, channel.end_time)
                            break
                else:
                    break
        
        # try to add request to last phase, it is separated block, because last phase doesnt have store
        for chanel in phases[n - 1].channels:
            if chanel.state == state_locked and any(chanel.state == state_free for chanel in phases[n].channels):
                for last_phase_chanel in phases[n].channels:
                    if last_phase_chanel.state == state_free:
                        last_phase_chanel.process_request(chanel.request, chanel.end_time)
                        chanel.release_request()

        if request_time <= system_time:
            request = Request(request_time)

            current_count += 1
                    # count statictic per system-time-step
            for phase in phases:
                phase.store.count_statistic()
                for channel in phase.channels:
                    channel.count_statistic()
            if any(channel.state == state_free for channel in phases[0].channels):
                for channel in phases[0].channels:
                    if channel.state == state_free:
                        channel.process_request(request, channel.end_time)
                        break

            elif len(phases[0].store.requests) < phases[0].store.max_size:
                phases[0].store.requests.append(request)

            request_interval = next(request_gen)
            request_time += max(request_interval, 0.1)

            if system_time <= request_time:
                system_time += 0.01

        else:
            system_time += 0.01
            
        if now_count != requests_count - current_count:
            now_count = requests_count - current_count
            fp.writelines("Current request: " + str(current_count))
            for i in range(len(phases)):
                fp.writelines("\n"+str(i+1)+" Phase: \n")
                fp.writelines("\tStorage: " + str(len(phases[i].store.requests)) + "\n")
                for j in range(len(phases[i].channels)):
                    fp.writelines("\tChannel " + str(j+1) + " state : " + str(phases[i].channels[j].state) + "\n")
            fp.writelines("====================================\n")
            

    fp.writelines("====================================\n")
    fp.writelines('\nRequests count: ' + str(requests_count) + "\n")
    fp.writelines('Rejection probability: ' + str(1 - len(out_requests) / requests_count) + "\n")
    fp.writelines('Statistic for each phase\n')
    for i in range(len(phases)):
        fp.writelines('\t' + str(i + 1) + ' Phase statistic:\n')
        fp.writelines('\tAverage storage size: ' + str(phases[i].store.statistic[0] / phases[i].store.statistic[1]) + "\n")
        for channel in phases[i].channels:
            free_prob = channel.statistic[0] / sum(channel.statistic)
            busy_prob = channel.statistic[1] / sum(channel.statistic)
            locked_prob = channel.statistic[2] / sum(channel.statistic)
            fp.writelines('\t\tFree channel (' + str(channel.statistic[0]) + '): ' + str(free_prob) + ' Busy channel (' + str(channel.statistic[1]) + '): ' + str(busy_prob) + ' Locked channel (' + str(channel.statistic[2]) + '): '+ str(locked_prob) + "\n")
    fp.close()
