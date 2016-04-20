(ns date-validation.core)

(import '(javax.swing JOptionPane))

(use '[clojure.string :only (join split)])

(defrecord Date[day month year])

(defn abs [n] (max n (- n)))

(defn split-date [date-string]
  (split date-string #"/")
)

(def daysInMonths {1 31 2 28 3 31 4 30 5 31 6 30 7 31 8 31 9 30 10 31 11 30 12 31})
(def monthNames {1 "January"
                 2 "February"
                 3 "March"
                 4 "April"
                 5 "May"
                 6 "June"
                 7 "July"
                 8 "August"
                 9 "September"
                 10 "October"
                 11 "November"
                 12 "December"})

(def daysInYear 365)
(def daysInLeap 366)

;;is function leap year?
(defn leapYear [year]
   
  ;;is it divisible by 4?
  (if (= (mod year 4) 0)
   (do

     ;;if divisible by 4, is it divisible by 100?
     (if (= (mod year 100) 0)

       ;;if divisible by 100, is it also divisible by 400?
       (if (= (mod year 400) 0)

         ;;if divisible by 400 and 100, then leap year
         true

         ;;if not, not leap year
         false
         )

       ;;if divisibly by 4 but not by 100, leap year
       true
       )
     )
    ;;if not divisible by 4, not leap year,
   false

    )
)

;;is function valid month?
(defn validMonth [month]

  (cond
    (<= month 0) false
    (> month 12) false
    :else true
    )
)

;;is function valid year?
(defn validYear [year]

  (cond 
    (<= year 0) false
    
    :else true
    )
)

;;is function valid date?
(defn validDay [date]

  (let [splitDate (split-date date)
        month (. Integer parseInt (nth splitDate 0))
        day (. Integer parseInt (nth splitDate 1))
        year (. Integer parseInt (nth splitDate 2))]

    (if (and (validMonth month) (validYear year))
      ;;if the month and year are valid
      (do
        (if (and (leapYear year) (= month 2))
          ;;if leap year and february
          (do
            ;;(println "leap year, feb")
            ;; if valid day
            (cond
              (<= day 0) false
              (> day 29) false
              :else true
              )
            )
          ;;else
          (do
            ;;if not leap year
            ;;(println "not leap year")
            (cond
              (<= day 0) false
              (> day (daysInMonths month)) false
              :else true
              )
            )
          )
        )
      )
    )
  )

;;generates list of years in between two dates
(defn years-between [year1 year2]

  (loop [result []
         current-num year1]
    (if (> current-num year2)
      result
      (recur (conj result current-num) (inc current-num))))
)

;;counts leap years between list of years
(defn count-leap-years [all-years]

  (let [leap-years (filter leapYear all-years)]

    ;;return the number of leap years
    (count leap-years)    
    )
)

;;gets days left in a year from a certain date
(defn get-days-left [date]

  (let [splitDate (split-date date)
        month (. Integer parseInt (nth splitDate 0))
        day (. Integer parseInt (nth splitDate 1))
        year (. Integer parseInt (nth splitDate 2))]

    (loop [result 0
           current-month month]
      (if (> current-month 12)
        (- result day)
        (recur (+ result (daysInMonths current-month)) (inc current-month)))

      )
    )
)

;;gets days past in a year from a certain date
(defn get-days-past [date]

  (let [splitDate (split-date date)
        month (. Integer parseInt (nth splitDate 0))
        day (. Integer parseInt (nth splitDate 1))
        year (. Integer parseInt (nth splitDate 3))]

    (loop [result 0
           current-month 1]
      (if (= current-month month)
        (+ result day)
        (recur (+ result (daysInMonths current-month)) (inc current-month)))

      )
    )
)

;;gets days in between two particular dates
(defn days-in-between [date1 date2]

  ;;are these valid dates?
  (if (and (validDay date1) (validDay date2))
    ;;if valid dates
    (do

      (let [splitDate1 (split-date date1)
            splitDate2 (split-date date2)

            month1 (. Integer parseInt (nth splitDate1 0))
            day1 (. Integer parseInt (nth splitDate1 1))
            year1 (. Integer parseInt (nth splitDate1 2))

            month2 (. Integer parseInt (nth splitDate2 0))
            day2 (. Integer parseInt (nth splitDate2 1))
            year2 (. Integer parseInt (nth splitDate2 2))

            ;;are the years equal

            ;;which year is later?
            later (max year1 year2)
            earlier (min year1 year2)

            years (years-between year1 year2)

            leap-years (count-leap-years years)

            days-left-earlier (get-days-left date1)
            
            days-past-later (get-days-past date2)

            num-days (+ (* 365 (- later (+ earlier 1))) leap-years days-past-later days-left-earlier)]

        (println num-days)
        )
      )
    ;;otherwise return nil
    nil
    )
  

)

;;lazy date string generators,
;;print out every single date from
;;passed in date til end of time

;;lazy-date-gen auxillary functions

(defn inc-date [date]

  ;;if the passed in date isn't valid
  (let [this-day-joined (join "/" 
                              [(date :month)
                               (date :day)
                               (date :year)])]
    (if (validDay this-day-joined)
      (do
        (let [next-day {:month (date :month)
                        :day (+ (date :day) 1)
                        :year (date :year)}

              next-day-joined (join "/" 
                                    [(next-day :month)
                                     (next-day :day)
                                     (next-day :year)])]

          (if (validDay next-day-joined)
            next-day-joined
            (do
              (let [next-day-2 {:month (+ 1 (date :month))
                                :day 1
                                :year (date :year)}
                    next-day-joined-2 (join "/" 
                                            [(next-day-2 :month) 
                                             (next-day-2 :day)      
                                             (next-day-2 :year)])]

                (if (validDay next-day-joined-2)
                  next-day-joined-2
                  (do
                    (let [next-day-3 {:month 1
                                      :day 1
                                      :year (+ 1 (date :year))}
                          next-day-joined-3 (join "/" 
                                                  [(next-day-3 :month)
                                                   (next-day-3 :day)
                                                   (next-day-3 :year)])]

                      (if (validDay next-day-joined-3)
                        next-day-joined-3
                        nil
                        )
                      )
                    )
                  )
                )              
              )
            )
          )
        )
      nil
      )
    )
)

;;condensed format generator
(defn get-dates [date]

  ;;validate the very first date
  (if (validDay date)
    ;;if valid go through steps
    (do

      ;;split up date into month day year
      (let [split-date (split date #"/")
            month (. Integer parseInt (nth split-date 0))
            day (. Integer parseInt (nth split-date 1))
            year (. Integer parseInt (nth split-date 2))
            date-map {:month month
                      :day day
                      :year year}
            date-map-joined (join "/"
                                  [(format "%02d" (date-map :month))
                                   (format "%02d" (date-map :day))
                                   (date-map :year)])]

        ;;add 0 to numbers less than 10...

        (lazy-seq
         ;;add current date to seq
         (cons date-map-joined
               ;;plan is to add incremented date to seq
               (get-dates (inc-date date-map))

               )
         )
        )
      )
    ;;else return nil
    nil
    )
)

;;long format generator
(defn get-dates-long [date]
  ;;validate the very first date
  (if (validDay date)
    ;;if valid go through steps
    (do

      ;;split up date into month day year
      (let [split-date (split date #"/")
            month (. Integer parseInt (nth split-date 0))
            day (. Integer parseInt (nth split-date 1))
            year (. Integer parseInt (nth split-date 2))
            date-map {:month month
                      :day day
                      :year year}
            date-map-joined (join
                             [(monthNames month)
                              " "
                              (format "02d" (date-map :day))
                              ", "
                              (date-map :year)])]


        (lazy-seq
         ;;add current date to seq
         (cons date-map-joined
               ;;plan is to add incremented date to seq
               (get-dates-long (inc-date date-map))

               )
         )
        )
      )
    ;;else return nil
    nil
    )

)

;;code for the sake of testing

(def dates '("4/4/2015" "1/4/2015" "4/6/2015" "2/29/2015" "2/29/2016" "11/1/1973"
               "4/31/2006" ))
