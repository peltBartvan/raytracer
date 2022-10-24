
newtype Ray
newtype Color
newtype Image

newtype Scene = Ray -> Color
newtype Observer = Scene -> Image

