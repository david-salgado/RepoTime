# RepoTime
This is an R package for the implementation of a notation for usual natural time intervals (months, terms, semesters, years, ...) as *PPp...p*, where 

* *PP* is:

  1. AA for years (*años* in Spanish);
  2. SS for semesters;
  3. TT for terms;
  4. MM for months;
  5. QQ for fortnights (*quincenas* in Spanish);
  6. DD for days;
  
  The notation shows a double character because in its application to data sets in statistical production we need occassionally to refer to rotated samples with respect to previous time intervals. In these cases, the notation sligthly changes to AR, SR, TR, MR and DR, respectively.
  
* *p...p* stands for concrete periods:

1. *yyyy* for years;
2. *syyyy* for semesters, where s=1,2;
3. *tyyyy* for terms, where t=1,2,3,4;
4. *mmyyyy* for months;
5. *fmmyyyy* for fortnights, where f=1,2;
5. *ddmmyyyy* for days.
 
The bottom line of the implementation is the definition of an S4 class named `RepoTimeInt` with two attributes (slots), that is, a character vector of time periods (intervals) with the aforementioned notation and a list with their corresponding time interval expressed as an object of class `Interval` of the package [lubridate](https://cran.r-project.org/web/packages/lubridate/).

Thus, the manipulation of these natural time periods is internally carried out through methods implemented in the lubridate package whose result is brought back to our notation.

The package is currently under development and may contain numerous bugs. It is being tested and under profiling. 
