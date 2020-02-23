# Kurallar
!duplicated(uyeno)
is.integer(uyeno)
uyeno > 1 & uyeno < 10000
is_within_range(uyeno, 1, 10000)

is.character(ad)
sapply(ad, is.character)

cins %in% c("E", "K")
is.character(cins)
sapply(cins, is.character)

is.integer(boy)
boy > 150 & boy < 250
is_within_range(boy, 150, 250)

is.numeric(kilo)
is_within_range(kilo, 50, 150)

is.character(sacrenk)
sacrenk %in% c("kumral", "siyah", "sarý", "kýzýl", "ak")

is.character(mhal)
is_proper_name(mhal)
mhal %in% c("Evli", "Bekar", "Boþanmýþ")

kilo/(boy/100) > 25 & kilo/(boy/100) < 100






