def convert_to_nepali(engdate, delta=-1):
    # date min 2000, push 17
    # date max 2090, Chaitra 30
    try:
        engdate = engdate.date()
    except:
        pass
    start_eng = datetime.date(1944, 1, 1)

    if delta==-1:
        delta_days = (engdate-start_eng).days

    else:delta_days=delta

    # nepali_date has array of months
    nep_yr = 2000
    nep_mt = 9
    nep_dy = 17

    ind = nep_yr - 2000
    mt = nep_mt-1
    rem_days = nepali_date[0][8] - nep_dy

    while True:
        delta_days -= rem_days
        #print('delta_days:', delta_days)
        mt+=1
        if delta_days == 0:
            #print('del 0 here')
            return (nep_yr, mt, nepali_date[ind][mt])
        if delta_days < 0:
            #print('neg here')
            return(nep_yr, mt, nepali_date[ind][mt-1]+delta_days)
        if mt>11:
            nep_yr+=1
            mt=0
            ind+=1
        rem_days = nepali_date[ind][mt]

def get_eng_date_range(nep_year, nep_month):
        start_eng_year = int(nep_year) - 57   # generally, engyr = nepyr-57
        month_ind = months.index(nep_month)+1
        if month_ind > 9: # but if month is > MAGH then nepyr-57+1
            start_eng_year+=1

        end_eng_year = start_eng_year
        start_eng_month = (month_ind+3)%13
        end_eng_month = start_eng_month+1
        if end_eng_month>12:
            end_eng_month=1
            end_eng_year += 1
        start_date = 15
        end_date = 20
        # we check from 15th of start month to 20th of next month so that we wont miss anything 

        # check first 6 days for our desired month
        nep = convert_to_nepali(datetime.datetime(start_eng_year, start_eng_month, start_date))
        # the month should not be equal to the month we seek( generally new month starts from 17/18)
        if nep[1]-1==month_ind:
            raise Http404('Something error with month calculation')
        for x in range(6):
            nep = convert_to_nepali(datetime.datetime(start_eng_year, start_eng_month, start_date+x))
            if nep[1]-1==month_ind:
                start_date = start_date+x
                break

        # now check last 6 days for our desired month
        nep = convert_to_nepali(datetime.datetime(end_eng_year, end_eng_month, end_date))
        # the month should not be equal to the month we seek( generally new month starts from 17/18)
        if nep[1]==month_ind:
            raise Http404('Something error with month calculation')
        for x in range(6):
            nep = convert_to_nepali(datetime.datetime(end_eng_year, end_eng_month, end_date-x))
            if nep[1]==month_ind:
                end_date = end_date-x
                break
        return [datetime.datetime(start_eng_year, start_eng_month, start_date).date(),
                    datetime.datetime(end_eng_year, end_eng_month, end_date).date()]

