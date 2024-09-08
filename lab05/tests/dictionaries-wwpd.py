test = {
  'name': 'Dictionaries WWPD',
  'points': 0,
  'suites': [
    {
      'cases': [
        {
          'code': r"""
          >>> pokemon = {'pikachu': 25, 'dragonair': 148}
          >>> pokemon
          a3197f1f63eacb2c950f99e64e8924a3
          # locked
          >>> 'mewtwo' in pokemon
          ac667055c8e3c84ad7260b0fefa2e007
          # locked
          >>> len(pokemon)  
          31f02e75f8bef5a0621b68131795447b
          # locked
          >>> pokemon['mew'] = pokemon['pikachu']   #If this errors, just type Error
          >>> pokemon[25] = 'pikachu'
          >>> pokemon
          d80e2d17f0b8d204b89350394cbf8d77
          # locked
          >>> pokemon['mewtwo'] = pokemon['mew'] * 2  
          >>> pokemon
          b79f3f5be2ec19fcebad789f2da5bdad
          # locked
          >>> pokemon[['firetype', 'flying']] = 146  # If this errors, just type Error. Note that dictionary keys must be hashable.
          66901ed5775b51743d745870a1a883e3
          # locked
          """,
          'hidden': False,
          'locked': True,
          'multiline': False
        }
      ],
      'scored': False,
      'type': 'wwpp'
    }
  ]
}
