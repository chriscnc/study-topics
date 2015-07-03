
fun valid_date d m =
  (d > 0) andalso
  (d < 29 andalso m = "February") orelse
  (d = 30 andalso (m = "April" orelse
                   m = "June" orelse
                   m = "September" orelse
                   m = "November")) orelse
  (d = 31 andalso (m = "January" orelse
                   m = "March" orelse
                   m = "May" orelse
                   m = "July" orelse
                   m = "August" orelse
                   m = "October" orelse
                   m = "December"))

        
