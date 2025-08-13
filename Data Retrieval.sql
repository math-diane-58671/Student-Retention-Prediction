/*
Author: Ruth Clayton
Title: Data Retrieval for Student Retention Analysis
Date: file created on 3/7/2025, finished on 5/21/2025.
Description: This is the query I wrote for ORU to get the real student data for this project.
*/

-- list of U.S. state codes in STVSTAT table in Banner
-- useful since there's a mix of foreign states in the validation table
-- and there's no flag to get only U.S.
with us_states_territories as (
    select
        stvstat_code
    from stvstat
    where stvstat_code in ('AA', 'AE', 'AK', 'AL', 'AP', 'AR', 'AS', 'AZ', 'CA', 'CO', 'CT', 'DC', 
    'DE', 'FL', 'GA', 'GU', 'HI', 'IA', 'ID', 'IL', 'IN', 'KS', 'KY', 'LA', 'MA', 'MD', 'ME', 'MI', 
    'MN', 'MO', 'MP', 'MS', 'MT', 'NC', 'ND', 'NE', 'NH', 'NJ', 'NM', 'NV', 'NY', 'OH', 'OK', 'OR', 
    'PA', 'PR', 'RI', 'SC', 'SD', 'TN', 'TX', 'UT', 'VA', 'VI', 'VT', 'WA', 'WI', 'WV', 'WY')
),
-- this is the student data pulled from my custom ruclayton.census table,
-- joined with another custom table, ruclayton.crosswalk, which contains the program mapping
-- I also make use of several 'organizational' functions from the oruf package, which 
-- contains functions to retrieve certain common student data points
initial_data as (select
    distinct
    -- student's unique identifier (Znumber, the student facing identification number)
    id,
    -- pidm is also a student unique identifier, but it is a database internal unique identifier
    -- (Znumber is only stored in spriden, pidm is used in every other table)
    spriden_pidm,
    -- student's classification code at census
    classification,
    -- student's admit term to their declared program at census
    admit_term,
    -- so, terms are stored as integers of 6 digits. The first four digits correspond to the year
    -- the 5th digit corresponds to the season, 1 = spring, 2 = summer, 3 = fall
    -- the 6th digit corresponds to the term number within the season
    case
        -- values of 1 means the student is entering
        -- if the season and year of the student's admit term matches the year and is in the fall, they are 'entering'
        when census_year || '3' = substr(admit_term,1,5) then 'Entering'
        else 'Returning'
    end as entering,
    -- sum of enrolled credit hours at the time of the fall census (summed across term numbers)
    credit_hrs,
    -- program description 
    (select major_crossed from ruclayton.crosswalk a
    where c.program_key = a.program_key) as program,
    -- level of the student's academic studies
    case when levl = 'NA' then null else levl end as levl,
    -- campus of the student's courses/of the student
    case when campus = 'NA' then null else campus end as campus,
    -- year when the census snapshot was taken
    census_year,
    -- admit code of the student, corresponds to an admit type
    case when admit_code = 'NA' then null else admit_code end as admit_code,
    -- this is the student's response to the separate ethnic question: hispanic or not hispanic
    oruf.get_ethn(spriden_pidm, 'HD') as ethnic,
    -- this is the student's response to race
    oruf.get_ethn(spriden_pidm, 'C') as old_ethnic,
    -- this is a system derived race based on country of origin - only used a last resort when
    -- no student identified values are available
    case
        when nvl(oruf.get_ethn(spriden_pidm,'RC'),'U') = 'A' then 'AS'
        when nvl(oruf.get_ethn(spriden_pidm,'RC'),'U') ='P' then 'AS'
        when nvl(oruf.get_ethn(spriden_pidm,'RC'),'U') ='HI' then 'X'
        when nvl(oruf.get_ethn(spriden_pidm,'RC'),'U') ='I' then 'AI'
        when nvl(oruf.get_ethn(spriden_pidm,'RC'),'U') ='F' then 'B'
        when nvl(oruf.get_ethn(spriden_pidm,'RC'),'U') ='L' then 'H'
        when nvl(oruf.get_ethn(spriden_pidm,'RC'),'U') ='V' then 'AS'
        when nvl(oruf.get_ethn(spriden_pidm,'RC'),'U') ='R' then 'X'
        else nvl(oruf.get_ethn(spriden_pidm,'RC'),'U')
    end as race,
    -- sex of the student, male or female
    case
        when oruf.get_gender(spriden_pidm) not in ('M', 'F') then null 
        else oruf.get_gender(spriden_pidm)
    end as gender,
    -- manipulating the year to match the format of the aid year code used in the Banner financial aid data
    SUBSTR(census_year, 3,2) || SUBSTR(census_year + 1, 3,2) as aidy_code,
    -- citizenship code of the student
    spbpers_citz_code as citz_code,
    -- comparing the state of the student's address to the U.S. states and territories list
    case 
        when oruf.get_address(spriden_pidm,'PR','S') = 'OK' then 'In-State'
        when oruf.get_address(spriden_pidm,'PR','S') in (select * from us_states_territories) then 'Out-of-State'
        when oruf.get_address(spriden_pidm, 'PR', 'N') != 'US' 
            and oruf.get_address(spriden_pidm, 'PR', 'N') is not null 
            and oruf.get_address(spriden_pidm,'PR','S') is null then 'Outside US'
        when oruf.get_address(spriden_pidm,'PR','S') is not null then 'Outside US'
        else null
    end as state_status,
    (select 
            case 
                -- this is really a data validation check. if the student is a U.S. citizen,
                -- then don't look at their nation of citizenship 
                when spbpers_citz_code = 'Y' then 'UNITED STATES OF AMERICA'
                else stvnatn_nation 
            end as stvnatn_nation 
    from stvnatn 
            -- set null values of natn_code_legal to ZZ, which corresponds to UNKNOWN in the stvnatn validation table
             where nvl(gobintl_natn_code_legal,'ZZ')
             = stvnatn_code) as nation_of_citizenship,
    -- calculated age of the student as it would have been at the start of the fall term 
    oruf.get_age(spriden_pidm, stvterm_start_date) as age_at_start_fall,
    -- the student's response to whether they are a first generation student on the application
    q.first_generation_student as first_generation,
    -- student's dorm status for the term
    case
        -- if dorm is not null and not CM then Dorm
        -- if campus != 1 then Online
        -- if dorm = CM then Commuter
        -- else null
        when oruf.get_dorm_code(spriden_pidm, census_year || '30') is not null 
            and oruf.get_dorm_code(spriden_pidm, census_year || '30') != 'CM' then 'Dorm'
        when campus != '1' then 'Online'
        when oruf.get_dorm_code(spriden_pidm, census_year || '30') = 'CM' then 'Commuter'
        else null
    end as residency
    from census c
    join spriden on id = spriden_id
    left outer join spbpers on spbpers_pidm = spriden_pidm
    left outer join gobintl on gobintl_pidm = spriden_pidm
    left outer join (select 
                        distinct
                        banner_id, 
                        first_generation_student
                    from salesforce.contact
                    where banner_id is not null
    ) q on spriden_pidm = banner_id
    join stvterm on stvterm_code = census_year || '30'
    -- exclude the concurrent, dual enrollment, and non-degree seeking students
    where admit_code not in ('C', 'DE', 'N')
),
loan_amounts as ( 
    select
        -- internal id
        rpratrm_pidm,
        -- financial aid year
        rpratrm_aidy_code,
        -- "Total Student Loan Amount in Census Aid Year"
        sum(rpratrm_offer_amt) as total_student_loan_in_season
    -- rpratrm is the student award table, a row for each award offered to each student in each aid year
    from rpratrm 
    join initial_data i on i.spriden_pidm = rpratrm_pidm 
        and i.aidy_code = rpratrm_aidy_code
    -- rfrbase is the validation table look up for all specific aid available (for example, "1st Restricted Scholarship" or
    -- "Unsubsidized Federal Stafford Loan")
    join rfrbase on rpratrm_fund_code = rfrbase_fund_code 
    -- include only records where the student accepted the loan (and where the loan amount is greater than 0)
    where rpratrm_offer_amt > 0
        -- include only records from the fall season
        and substr(rpratrm_period,5,1) = '3'
        -- include all records with LOAN type only
        and rfrbase_ftyp_code = 'LOAN'
        -- exclude records that have the federal fund id of PLUS, that 
        -- corresponds to parent loans
        and rfrbase_fed_fund_id != 'PLUS'
    group by rpratrm_pidm, rpratrm_aidy_code
),
income_category as (
    -- this information comes from how the student responds on their FAFSA application for the aid year
    -- in the case where the student is a dependent, this value corresponds to their parents/guardians
    select
        rcrapp1_pidm,
        rcrapp1_aidy_code,
        case
            -- this income category logic reflects the income categories IPEDS uses 
            when rcrapp4_fisap_inc > 110000 then '$110,001 and more'
            when rcrapp4_fisap_inc > 75000 and rcrapp4_fisap_inc <= 110000 then '$75,001-110,000'
            when rcrapp4_fisap_inc > 48000 and rcrapp4_fisap_inc <= 75000 then '$48,001-75,000'
            when rcrapp4_fisap_inc > 30000 and rcrapp4_fisap_inc <= 48000 then '$30,001-48,000'
            when rcrapp4_fisap_inc >= 0 and rcrapp4_fisap_inc <= 30000 then '$0-30,000'
            else null
        --"Family Income Category"
        end as family_income_category_in_aidy
    from initial_data i
    join rcrapp1
        on i.spriden_pidm = rcrapp1_pidm
        and i.aidy_code = rcrapp1_aidy_code
    join rcrapp4
        on i.spriden_pidm = rcrapp4_pidm
        and i.aidy_code = rcrapp4_aidy_code
        and rcrapp1_seq_no = rcrapp4_seq_no
        and rcrapp1_infc_code = rcrapp4_infc_code
    where rcrapp1_curr_rec_ind = 'Y'
    and rcrapp1_infc_code = 'EDE'
),
-- distance between offer date and start of term
scholarships as (
    select
        rpratrm_pidm as rpratrm_pidm2,
        rpratrm_aidy_code as rpratrm_aidy_code2,
        -- weighted average date of offer
        round(months_between((date '1970-01-01'
        + ( SUM( (rpratrm_orig_offer_date - date '1970-01-01') * rpratrm_orig_offer_amt )
            / SUM(rpratrm_orig_offer_amt) )), s.stvterm_start_date))           as months_between_weighted_offer_date,
        -- months between last offer date and the term start date
        round(months_between(max(rpratrm_orig_offer_date), s.stvterm_start_date), 1) as months_between_term_and_last_offer,  
        -- months between first offer date and the term start date
        round(months_between(min(rpratrm_orig_offer_date), s.stvterm_start_date), 1) as months_between_term_and_first_offer,
        --"Total Scholarships/Grants/Work Study/Tuition Benefits"
        sum(rpratrm_orig_offer_amt) as total_scholarships_in_season
    from rpratrm
    join rfrbase on rpratrm_fund_code = rfrbase_fund_code
    join initial_data on rpratrm_pidm = spriden_pidm    
        and rpratrm_aidy_code = aidy_code 
    join (select *
            from stvterm
        where substr(stvterm_code,6,1) = '0') s
        on rpratrm_period = s.stvterm_code
    where rpratrm_orig_offer_amt > 0
        -- include only records from the fall season
        and substr(rpratrm_period,5,1) = '3'
        -- these are all of the types of gift aid
        and rfrbase_ftyp_code in ('GRNT','REST','SCHL','OTHR', 'WORK', 'BENE')
    group by rpratrm_pidm, rpratrm_aidy_code, s.stvterm_start_date
),
pell_grant as (
    -- this is meant to be a binary indicator, 1 if the student received a pell grant,
    -- 0 if the student did not receive a pell grant
    -- the amount of the pell grant would be captured in the scholarships CTE
    select distinct 
        rpratrm_pidm as rpratrm_pidm3, 
        rpratrm_aidy_code as rpratrm_aidy_code3,
        1 as pell_grant_recipient
    from initial_data i
    join rpratrm on i.spriden_pidm = rpratrm_pidm 
        and i.aidy_code = rpratrm_aidy_code
    join rfrbase on rpratrm_fund_code = rfrbase_fund_code 
    where rpratrm_offer_amt > 0
    -- include only records from the fall season
    and substr(rpratrm_period,5,1) = '3'
    and rfrbase_fund_code = 'PELL'),
cleaned_gpa as (
    select 
        sorhsch_pidm, 
        --sorhsch has some interesting (i.e. completely invalid) high school GPA values
        -- in a separate query, I've explored the possible invalid values and came up with these cleaning steps
        case 
            -- some sorhsch_gpa values are stored as three dashes, or as one or more white spaces
            -- we want to exclde these values
            -- the sorhsch_sbgi_code is the high school/institution code that corresponds to a description in the stvsbgi 
            -- table. 10000A corresponds to G.E.D. - unfortunately, G.E.D. scores are stored under the sorhsch_gpa field; 
            -- since that's a different scale than gpa, we exclude the GED records and later flag them
            when sorhsch_gpa = '---' or TRIM(sorhsch_gpa) is null or sorhsch_sbgi_code = '10000A' then null
            -- some GPA values have a valid GPA, but they're surrounded by a variable amount of white spaces, which is
            -- why we need to trim and then convert back to a number
            else TO_NUMBER(TRIM(sorhsch_gpa))
        end as gpa,
        extract(month from sorhsch_graduation_date) as high_school_grad_month,
        extract(year from sorhsch_graduation_date) as high_school_grad_year,
        sorhsch_activity_date,
        -- flag the GED records
        case when sorhsch_sbgi_code = '10000A' then 1 else 0 end as ged_flag,
        -- flag to track students that had an invalid non-null gpa
        case 
            when sorhsch_sbgi_code != '10000A' 
                 and sorhsch_gpa is not null 
                 and sorhsch_gpa != '---'
                 -- both weighted and unweighted GPAs are stored in the same field... 
                 -- there's no way to separate them, so the best I can do is flag and later filter out unlikely values
                 and (TO_NUMBER(TRIM(sorhsch_gpa)) < 0.5 or TO_NUMBER(TRIM(sorhsch_gpa)) > 6)
            then 1 else 0 
        end as invalid_gpa_flag
    from initial_data i 
    join sorhsch on sorhsch_pidm = i.spriden_pidm
),
valid_gpa as (
    select 
        sorhsch_pidm,
        gpa,
        high_school_grad_month,
        high_school_grad_year,
        sorhsch_activity_date
    from cleaned_gpa
    where gpa is not null
      -- filter out unlikely gpa values
      and gpa between 0.5 and 6.0
),
ranked_gpa as (
    -- the purpose of this CTE is to order the gpa values by the most recent activity date
    -- it's possible for students to have multiple sorhsch records
    select 
        sorhsch_pidm,
        gpa,
        high_school_grad_month,
        high_school_grad_year,
        sorhsch_activity_date,
        row_number() over (partition by sorhsch_pidm order by sorhsch_activity_date desc) as rnk
    from valid_gpa
),
most_recent_gpa as (
    -- here is where we keep only the most recent high school gpa record for each student
    select
        sorhsch_pidm,
        gpa,
        high_school_grad_month,
        high_school_grad_year,
        sorhsch_activity_date
    from ranked_gpa
    where rnk = 1
),
ged_flags as (
    -- since there can be multiple records for the student, 
    -- we select the max of the ged_flag and then if any of the records are a positive ged_flag, 
    -- it will be caught
    select 
        sorhsch_pidm,
        max(ged_flag) as has_ged
    from cleaned_gpa
    group by sorhsch_pidm
),
invalid_flags as (
    -- since there can be multiple records for the student, 
    -- we select the max of the invalid flag and then if any of the records are a positive flag, 
    -- it will be caught
    select 
        sorhsch_pidm,
        max(invalid_gpa_flag) as had_invalid_gpa
    from cleaned_gpa
    group by sorhsch_pidm
),
all_students as (
    -- the point of this CTE is to capture all students who had any kind of sorhsch record
    select sorhsch_pidm from most_recent_gpa
    union
    select sorhsch_pidm from ged_flags
    union
    select sorhsch_pidm from invalid_flags
),
high_school_data as (
    -- the purpose of this CTE is to combine the sorhsch information about the student 
    -- so that there is one record per student, regardless of if they have GED record, a invalid gpa record,
    -- and/or a valid gpa record
    select 
        a.sorhsch_pidm,
        g.gpa,
        g.sorhsch_activity_date,
        g.high_school_grad_month,
        g.high_school_grad_year,
        f.has_ged,
        i.had_invalid_gpa
    from all_students a
    left join most_recent_gpa g on a.sorhsch_pidm = g.sorhsch_pidm
    left join ged_flags f on a.sorhsch_pidm = f.sorhsch_pidm
    left join invalid_flags i on a.sorhsch_pidm = i.sorhsch_pidm),
all_adv as (
    -- this is to capture any students who were ever declared a concurrent student type
    -- the reason for this is that historically, students did not have to enroll in the concurrent part of term
    -- they could have been enrolled in any part of term as a concurrent student
    select 
        distinct
        sgbstdn_pidm as adv_pidm,
        1 as advantage
    from sgbstdn
    where sgbstdn_styp_code = '7'
),
-- advantage_courses as (
--     -- this is the capture any students who ever enrolled and stay enrolled or withdrew
--     -- from a concurrent class in the concurrent part of term before their enrollment at the census date
--     select 
--         distinct 
--         sfrstcr_pidm as adv_pidm,
--         1 as advantage
--     from initial_data i
--     join sfrstcr on i.spriden_pidm = sfrstcr_pidm
--     where sfrstcr_rsts_code in ('RE', 'WF', 'WP')
--         and sfrstcr_ptrm_code = 'ADV'
--         and to_number(substr(sfrstcr_term_code,1,4)) <= census_year
--         ),
-- all_adv as (
--     -- to capture all the past advantage students
--     select * from previous_adv_students
--     union all
--     select * from advantage_courses
-- ),
pave_student as (
    -- The pave program is a program to help students who start at a disadvantage (i.e. lower high school gpa)
    -- pave students are tracked through student attributes in sgrsatt
    -- since we are dealing with historical data, the current sgrsatt attributes wouldn't be appropriate/accurate,
    -- which is why saraatt is used here instead
    select 
        distinct
        saraatt_pidm, 
        1 as started_pave
    from initial_data i
    -- the saraatt table records any changes to student attributes
    join saraatt on i.spriden_pidm = saraatt_pidm
        -- want to ensure the change in attribute occurred during or before the fall semester in question
        and saraatt_term_code <= i.census_year || '39' 
    -- atts_code BRID is the attribute for PAVE students
    where saraatt_atts_code = 'BRID'
),
-- students who are planning to enroll for fall 2025
planned_fall2025 as (
    select 
        twbvaln_pidm as id
    from twbvaln
    where twbvaln_term_code like '20253%'
        and oruf.is_enrolled_by_date(twbvaln_pidm, twbvaln_term_code, sysdate) = 'Y'
        and oruf.is_enrolled_by_date(twbvaln_pidm, twbvaln_term_code, sysdate, 'Y') = 'N'
        and twbvaln_valid_code in (
          select twvvaln_valid_code
          from twvvaln
          where twvvaln_active_ind = 'Y'
            and twvvaln_valid_status = 'Y'
      )
),
graduation as (
    -- since retention also includes students who graduate before the next fall semester,
    -- we need to check for graduates
    select
        distinct
        shrdgmr_pidm as id,
        -- IPEDS considers a student graduated before a fall semester if they graduate on or before August 31st
        case 
            -- to check if student was retained for a specific fall to fall season,
            -- the year of graduation will only match if the day/month is after August 31
            -- for example, if a student was enrolled in Fall 2023 and graduated December 2023, the years match and we count
            -- that student as retained
            -- but if a student was enrolled in Fall 2023 and graduated May 2024, the years don't match, but we still want to count
            -- that student as retained for the 2023 year
            when to_char(shrdgmr_grad_date, 'MMDD') > '0831'
                then extract(year from shrdgmr_grad_date)
            -- that's why if the graduation month/day is not between 0831 and 1231, we need to subtract 1 from the year
            else extract(year from shrdgmr_grad_date) - 1
        end as census_year
    from shrdgmr
    -- include any award (all degs_code starting with A are awarded records -- the student can complete a certificate and that would count
    -- as retained for this definition)
    where shrdgmr_degs_code like 'A%'
        -- the records we are using start in 2017, so only graduations from August 31, 2017 and on will matter
        and shrdgmr_grad_date >= TO_DATE('31-AUG-2017', 'DD-MM-YYYY')
    ),
retention_oru as (
  select 
    -- this is the target variable
    -- we check for the student record in the next census year or if they graduated before the next fall
    e.spriden_pidm as e_id,
    e.census_year as e_census_year,
    -- 1 = retained, 0 = not retained
    case 
      when e.census_year < 2024 then 
        case 
          when e_next.spriden_pidm is not null or g.id is not null then 1
          else 0
        end
      when e.census_year = 2024 then 
        case 
          when g.id is not null or p.id is not null then 1
          else 0
        end
      else 0
    end as retained_to_oru
  from initial_data e
  left join initial_data e_next
    on e.id = e_next.id
    and e.census_year + 1 = e_next.census_year
  left join graduation g 
    on e.spriden_pidm = g.id
    and e.census_year = g.census_year
  left join planned_fall2025 p
    on e.spriden_pidm = p.id
    and e.census_year = 2024
),
transfer_hours as (
    -- shrtgpa contains hours that count towards your GPA... and it contains hours that are transfer hours
    -- it's the easiest way to aggregate transfer hours that actually count towards the student's academic record
    select 
        shrtgpa_pidm,
        census_year as census_year4,
        sum(shrtgpa_hours_earned) as transfer_hour
    from shrtgpa 
    join initial_data on spriden_pidm = shrtgpa_pidm 
        and levl = shrtgpa_levl_code
    --gpa_type_ind T is for transfer hours
    where shrtgpa_gpa_type_ind = 'T'
    -- only includes transfer hours for during or before the fall semester
    and shrtgpa_term_code <= census_year || '32'
    group by shrtgpa_pidm, census_year
),
term_cum_gpa as (
    -- 
    select
        shrtgpa_pidm as shrtgpa_pidm2,
        census_year as census_year6, 
        case 
            -- this is to prevent dividing by 0. If there is a record in shrtgpa but the shrtgpa_hours are 0,
            -- that means that the student may have taken 0 credit hour classes or not received credit for classes
            when sum(shrtgpa_gpa_hours) = 0 then 0
            -- quality points essentially correlate to the grades the student received for their classes. 
            else sum(shrtgpa_quality_points)/sum(shrtgpa_gpa_hours)
        end as term_cum_gpa
    from shrtgpa
    join initial_data on spriden_pidm = shrtgpa_pidm 
        -- only want to calculate the cumulative term gpa for credits on the same level as the student at census
        and levl = shrtgpa_levl_code
    -- include transfer and institutional gpa types
    where shrtgpa_gpa_type_ind in ('I','T')
      -- only include terms up to the summer before the census year
      and (shrtgpa_term_code <= census_year || '29' 
            -- or when the student transfers in credits before matriculating, which is indicated by term code 100000
            or shrtgpa_term_code = '100000')    
    group by shrtgpa_pidm, census_year
),
returning_students_with_missing_for_term_cum_gpa as (
    -- there is a bit of a student level type error present in the historical data for Master in Divinity students and
    -- Doctor of Ministry students. There is a group of returning student records (~200) where the student's level 
    -- doesn't match the shrtgpa level of their classes. these two programs in particular, there is the option for a student
    -- take it as a professional degree or as a traditional graduate degree, but the level of the courses for the programs remain
    -- the same, which is what causes the type mismatch
    -- to address this, first identify the returning students who did not have a term cum gpa calculated above
    select
        spriden_pidm,
        census_year,
        admit_term
    from initial_data
    where entering = 'Returning'
    and not exists (
        select 1
          from term_cum_gpa t
        where t.shrtgpa_pidm2 = spriden_pidm
            and t.census_year6 = census_year
      )    
),
term_cum_gpa_for_missing_returning as (
    -- now, instead of matching the student level to the course level for these students, 
    -- 
    select
        shrtgpa_pidm as shrtgpa_pidm4,
        i.census_year as census_year9,
        case 
            when sum(shrtgpa_gpa_hours) = 0 then 0
            else sum(shrtgpa_quality_points)/sum(shrtgpa_gpa_hours)
        end as alt_term_cum_gpa
    from shrtgpa s
    join returning_students_with_missing_for_term_cum_gpa i 
        on i.spriden_pidm = s.shrtgpa_pidm
    where s.shrtgpa_gpa_type_ind in ('T', 'I')
      and s.shrtgpa_term_code < i.census_year || '29'  
      and s.shrtgpa_term_code >= admit_term
    group by shrtgpa_pidm, i.census_year    
),
term_cum_gpa_ug as (
    select
        shrtgpa_pidm as shrtgpa_pidm3,
        census_year as census_year7, 
        case 
            when sum(shrtgpa_gpa_hours) = 0 then 0
            else sum(shrtgpa_quality_points)/sum(shrtgpa_gpa_hours)
        end as term_cum_gpa_ug
    from shrtgpa
    join initial_data on spriden_pidm = shrtgpa_pidm 
    where shrtgpa_gpa_type_ind in ('I','T')
      and shrtgpa_levl_code = 'UG'
      and (shrtgpa_term_code <= census_year || '29' or shrtgpa_term_code = '100000')    
    group by shrtgpa_pidm, census_year
),
credit_hours_per_season as (
    select 
        sfrstcr_pidm,
        census_year,
        substr(sfrstcr_term_code,1,5) as season,
        sum(sfrstcr_credit_hr) as credit_hrs
    from initial_data i
    join sfrstcr on i.spriden_pidm = sfrstcr_pidm
    where sfrstcr_rsts_code in ('RE', 'WF', 'WP')
        and sfrstcr_term_code <= census_year || '39'
        and sfrstcr_levl_code = levl
    group by sfrstcr_pidm, census_year, substr(sfrstcr_term_code,1,5)
),
credit_hour_velocity as (
    select
        sfrstcr_pidm as sfrstcr_pidm8,
        census_year as census_year8,
        avg(credit_hrs) as credit_hour_velocity
    from credit_hours_per_season
    group by sfrstcr_pidm, census_year
),
all_data as (
    select
    distinct 
    id,
    spriden_pidm,
    census_year,
    months_between_term_and_first_offer,
    months_between_term_and_last_offer,
    months_between_weighted_offer_date,
    credit_hour_velocity,
    alt_term_cum_gpa,
    term_cum_gpa,
    term_cum_gpa_ug,
    entering,
    transfer_hour,
    classification,
    program,
    nation_of_citizenship,
    levl,
    campus,
    admit_code,
    high_school_grad_month,
    high_school_grad_year,
    case 
        when citz_code in ('PP', 'N', 'EN') or old_ethnic = 'N' then 'Non U.S. Citizen'
        when (ethnic = 'Hispanic' or old_ethnic in ('L', 'H') or ((old_ethnic is null or old_ethnic = 'U') 
            and ethnic = 'Unknown' and race = 'H')) then 'Hispanic'
        when (old_ethnic in ('AI', 'I') or ((old_ethnic is null or old_ethnic = 'U') and race = 'AI')) 
            then 'American Indian/Alaska Native'
        when (old_ethnic in ('AS', 'A') or ((old_ethnic is null or old_ethnic = 'U') and race = 'AS')) 
            then 'Asian'
        when (old_ethnic in ('P', 'HI') or ((old_ethnic is null or old_ethnic = 'U') and race = 'AI')) 
            then 'Hawaiian/Pacific Islander'
        when (old_ethnic in ('B', 'F') or ((old_ethnic is null or old_ethnic = 'U') and race = 'B')) 
            then 'Black/African American'
        when (old_ethnic in ('M')) then 'Two or more races'
        when (old_ethnic in ('W') or ((old_ethnic is null or old_ethnic = 'U') and race = 'W')) 
            then 'White'
        else null
    end as ipeds_race,
    gender,
    citz_code,
    case 
        when first_generation = 'Yes' then 1
        else 0
    end as first_generation,
    state_status,
    age_at_start_fall,
    admit_term,
    credit_hrs,
    residency,
    nvl(total_student_loan_in_season, 0) as total_student_loan_in_season,
    family_income_category_in_aidy,
    nvl(total_scholarships_in_season, 0) as total_scholarships_in_season,
    nvl(pell_grant_recipient, 0) as pell_grant_recipient,
    gpa as hs_gpa,
    has_ged,
    had_invalid_gpa,
    nvl(advantage, 0) as advantage,
    nvl(started_pave, 0) as started_pave,
    retained_to_oru 
    from initial_data
    left join loan_amounts
        on spriden_pidm = rpratrm_pidm
        and aidy_code = rpratrm_aidy_code
    left join income_category
        on spriden_pidm = rcrapp1_pidm
        and aidy_code = rcrapp1_aidy_code
    left join scholarships on 
        spriden_pidm = rpratrm_pidm2
        and aidy_code = rpratrm_aidy_code2
    left join pell_grant on
        spriden_pidm = rpratrm_pidm3
        and aidy_code = rpratrm_aidy_code3
    left join high_school_data on
        spriden_pidm = sorhsch_pidm
    left join all_adv on
        spriden_pidm = adv_pidm
    left join pave_student on 
        spriden_pidm = saraatt_pidm
    left join retention_oru on
        spriden_pidm = e_id and 
        census_year = e_census_year
    left join transfer_hours on
        spriden_pidm = shrtgpa_pidm
        and census_year = census_year4
    left join term_cum_gpa on
        spriden_pidm = shrtgpa_pidm2
        and census_year = census_year6
    left join term_cum_gpa_ug on
        spriden_pidm = shrtgpa_pidm3
        and census_year = census_year7
    left join credit_hour_velocity on
        spriden_pidm = sfrstcr_pidm8
        and census_year = census_year8
    left join term_cum_gpa_for_missing_returning 
        on spriden_pidm = shrtgpa_pidm4
        and census_year = census_year9),
id_replacement as (
    select 
        id as banner_id,
        census_year as census_year10,
        row_number() over (order by id) as new_id
    from all_data
)
select
 new_id,
 census_year,
 months_between_term_and_first_offer as first_offer,
 months_between_term_and_last_offer as last_offer,
 months_between_weighted_offer_date as weighted_offer,
 round(credit_hour_velocity, 2) as credit_hour_velocity,
 nvl(advantage, 0) as advantage,
 entering,
 coalesce(round(term_cum_gpa,3), round(alt_term_cum_gpa,3)) as term_cum_gpa_same_level,
 round(term_cum_gpa_ug, 3) as term_cum_gpa_ug,
 nvl(transfer_hour,0) as transfer_hours,
 admit_term,
 credit_hrs,
 classification,
 program,
 nation_of_citizenship,
 case 
    when levl in ('CD', 'UG') then 'Undergraduate'
    when levl is not null then 'Graduate'
    else null
 end as levl,
 -- residential is 1, all else (i.e. online) is 0
 case 
    when campus = '1' then 1
    when campus is not null then 0
    else null
 end as campus,
 admit_code,
 high_school_grad_month,
 high_school_grad_year,
 ipeds_race,
 case
    when gender = 'F' then 1
    when gender = 'M' then 0
    else null
 end as gender,
 case 
    when citz_code in ('Y', 'PR') then 1
    when citz_code in ('EN', 'PP', 'N') then 0
    else null
 end as citz_code,
 nvl(first_generation, 0) as first_generation,
 state_status,
 age_at_start_fall,
 residency,
 nvl(total_student_loan_in_season, 0) as total_student_loan_in_season,
 family_income_category_in_aidy,
 nvl(total_scholarships_in_season, 0) as total_scholarships_in_season,
 nvl(pell_grant_recipient, 0) as pell_grant_recipient,
 hs_gpa,
 nvl(has_ged, 0) as has_ged,
 nvl(had_invalid_gpa,0) as had_invalid_gpa,
 nvl(started_pave, 0) as started_pave,
 retained_to_oru
 from all_data
 join id_replacement on id = banner_id and census_year = census_year10
;


