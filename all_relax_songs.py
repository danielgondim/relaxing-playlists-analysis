import os, csv
from glob import glob

path = '/home/danielgondim/workspace-new/phd/experiments/qualificacao/8tracks_pls_rlx_and_nrlx/relax'
output_path = '/home/danielgondim/workspace-new/phd/experiments/qualificacao/tese/relax_data_to_gmm.csv'

for user in os.listdir(path):
    current_user = glob(path + '/' + user + "/*.csv")
    print current_user
    for pl in current_user:
        with open(pl, 'rb') as playlist:
            reader = csv.reader(playlist)
            header = reader.next()
            for row in reader:
                row = row[0:8] + row[9:11]
                with open(output_path, 'a') as output:
                    writer = csv.writer(output)
                    writer.writerow(row)