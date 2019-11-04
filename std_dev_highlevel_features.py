import statistics, os, csv
from glob import glob

PLAYLISTS_8TRACKS = ['/home/danielgondim/workspace-new/phd/experiments/qualificacao/8tracks_pls_rlx_and_nrlx/relax','/home/danielgondim/workspace-new/phd/experiments/qualificacao/8tracks_pls_rlx_and_nrlx/not_relax']

def features_stdev(playlists):
    result = [[],[],[],[],[],[],[],[],[]]
    for playlist in playlists:
        with open(playlist, 'rb') as user_pls:
            reader = csv.reader(user_pls)
            header = reader.next()
            for row in reader:
                row = row[1:8] + row[9:11]
                row = map(float, row)
                for feature in range(len(row)):
                    result[feature].append(row[feature])
    if len(result[0]) == 1:
        return [0,0,0,0,0,0,0,0,0]
    else:
        return [statistics.stdev(x) for x in result]

to_append = []
for mood in PLAYLISTS_8TRACKS:
    for user in os.listdir(mood):
        current_user = glob(mood + "/" + user + "/*.csv")
        to_append.append(features_stdev(current_user))
        print current_user

with open('/home/danielgondim/workspace-new/phd/experiments/qualificacao/logistic_mfcc_8tracks.csv', 'rb') as perception:
    reader = csv.reader(perception)
    header = reader.next()
    counter = 0
    for row in reader:
        row += to_append[counter]
        counter += 1
        with open('/home/danielgondim/workspace-new/phd/experiments/qualificacao/logistic_stdev_8tracks.csv', 'a') as output:
            writer = csv.writer(output)
            writer.writerow(row)
