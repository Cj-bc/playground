module Collision where

import Data.Vector3

data Collision = Collision { }

data CollisionWorld = CollisionWorld [Collision]


data Transform = Transform_Sphere { center :: Vector3 }

data Shape = Sphere { sphere_radius :: Float }
           | Capsule { capsule_radius :: Float }
           | AABB { aabb_min :: Vector3
                  , aabb_max :: Vector3 }
           | OBB
           | DOP
           | ConvexPolyhedron
           | PolygonSoup
           | MultiShape [Shape]

data Collidable = Collidable Shape Transform
