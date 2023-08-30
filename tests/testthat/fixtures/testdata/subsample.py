#!/usr/bin/env python3
# coding=utf-8
"""..."""
__author__ = 'Simon J. Greenhill <simon@simon.net.nz>'
__copyright__ = 'Copyright (c) 2023 Simon J. Greenhill'
__license__ = 'New-style BSD'

import csvw
from collections import Counter

def get(filename):
    with csvw.UnicodeDictReader(filename) as reader:
        for row in reader:
            yield(row)

def put(filename, records):
    with csvw.UnicodeWriter(filename) as writer:
        header = records[0].keys()
        writer.writerow(header)
        for o in records:
            writer.writerow([o[h] for h in header])

# keep languages from Worroran
# ... because there are only 15 of them and there are some dialects.

#Family = worr1236

language_ids = [
    'gamb1251',
    'kwin1241',
    'miwa1242',
    'ngar1284',
    'munu1238',
    'ngar1285',
    'worr1237',
    'yawi1239',
]

if __name__ == '__main__':
    keep = []
    # for row in get('languages.csv'):
    #     if row['ID'] in language_ids:
    #         keep.append(row)
    #
    # put('languages.csv', keep)

    tally = Counter()
    for row in get('values.csv'):
        if row['Language_ID'] in language_ids:
            row['Source'] = 'testsource'
            keep.append(row)
            tally[row['Language_ID']] += 1

    put('values.csv', keep)

    # for t in tally.most_common():
    #     print(t)
