test = {
  'name': 'Lists',
  'points': 0,
  'suites': [
    {
      'cases': [
        {
          'code': r"""
          >>> s = [7//3, 5, [4, 0, 1], 2]
          >>> s[0]
          61b793952531daad90d65377b695da99
          # locked
          >>> s[2]
          176f11e2ea330c098313db1f1576967f
          # locked
          >>> s[-1]
          61b793952531daad90d65377b695da99
          # locked
          >>> len(s)
          e6efc1fcfbebed28c5068a807b6cce64
          # locked
          >>> 4 in s
          61e74011ca20035e5cb51b814087a093
          # locked
          >>> 4 in s[2]
          46d1f016b6482a76a74835354edaab71
          # locked
          >>> s[2] + [3 + 2]
          155bdce448ea76d920b83d1dcf697d0e
          # locked
          >>> 5 in s[2]
          61e74011ca20035e5cb51b814087a093
          # locked
          >>> s[2] * 2
          2ada3a8823635a1d6945f06fab16565d
          # locked
          >>> list(range(3, 6))
          ac7755cdf1ff110634a6d34c9ed8e5cd
          # locked
          >>> range(3, 6)
          8c6f6a2daaf343333ab8c3e4e827dbd2
          # locked
          >>> r = range(3, 6)
          >>> [r[0], r[2]]
          b48590a202f87b02895e3dd9fb48724b
          # locked
          >>> range(4)[-1]
          62cb7be5b3f27b8761401e9f99897a30
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
