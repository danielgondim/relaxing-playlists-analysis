import csv

with open('/home/danielgondim/workspace-new/phd/experiments/qualificacao/8tracks_relax_stdev.csv', 'rb') as relax_8tracks:
    reader = csv.reader(relax_8tracks)
    header = reader.next()
    for row in reader:
        if row[-1] != '0':
            with open('/home/danielgondim/workspace-new/phd/experiments/qualificacao/8tracks_relax_stdev_many_songs.csv', 'a') as output:
                writer = csv.writer(output)
                writer.writerow(row)