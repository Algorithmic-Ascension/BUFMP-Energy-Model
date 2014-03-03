BUFMP-Energy-Model
==================

Initial commit.
    I was working on this and it was spaghetti code before on 'mystery.R', but I haven't gotten around to cleaning it up. Instead I will start anew on 'final model.R', and copy the relevant code, editing into good code as I go along. I still need to learn how data frames deals with dates and actually using isodates instead of hacking it into days since 1970-01-01 used when converting into data matrix.

The model:
    energy = llr(temperature) + f(occupancy) + HotW * epsilon

    - llr is local linear regression
    - HotW is hour of the week, as measured by maximum of the density using density estimation with bandwidth selection method SJ. assuming we have enough space between peaks, we can use a normal approximation and do inference over it.
    - we can guestimate regular occupancy, I'm thinking f might be logarithmic 

TODO:
    - use data matrix to clean up code
    - add the relevant files to repo
