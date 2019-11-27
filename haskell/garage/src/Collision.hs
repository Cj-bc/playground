module Collision where


data Collision = Collision { }

data CollisionWorld = CollisionWorld [Collision]

data Vector3 = Vector3 Int Int Int

data Transform = Transform_Sphere { center :: Vector3 }

data Shape = Sphere { sphere_radius :: Int }
           | Capsule { capsule_radius :: Int }
           | AABB { aabb_min :: Vector3
                  , aabb_max :: Vector3 }
           | OBB
           | DOP
           | ConvexPolyhedron
           | PolygonSoup
           | MultiShape [Shape]

data Collidable = Collidable Shape Transform
