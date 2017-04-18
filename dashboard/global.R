site_lookup = list(
    #'ALL' = str_extract(datafeed_get_files(), '.*(?=\\.csv)'),
    'New York' = c(
        'MESA9',
        'MESA10',
        'MESA11',
        'MESA13',
        'MESA14'
        ),
    'Los Angeles' = c(
        'MESA1',
        'MESA2',
        'MESA3',
        'MESA4',
        'MESA5'
        ),
    'Winston-Salem' = c(
        'MESA6',
        'MESA7',
        'MESA17',
        'MESA19'
        ),
    'Baltimore' = c(
        'MESA20',
        'MESA21',
        'MESA23',
        'MESA24',
        'MESA28',
        'MESA30'
        ),
    'Saint Paul' = c(
        'MESA26',
        'MESA31',
        'MESA32',
        'MESA34'
        ),
    'Chicago' = c(
        'MESA12',
        'MESA15',
        'MESA25',
        'MESA27',
        'MESA36',
        'MESA37'
        )
    )
