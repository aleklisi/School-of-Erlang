CREATE
    (a1:Supervisor { name: 'supervisor' }),
    (b1:Worker { name: 'alpha' }),
    (b2:Worker { name: 'beta' }),
    (b3:Worker { name: 'gamma' })

MATCH (s:Supervisor), (w:Worker)
CREATE (s)-[r:Supervises]->(w)

MATCH (n) RETURN n
