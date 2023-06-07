There are six parts to this project:

1. "register_pckg" - The ability to register a new package in the system, adding a uuid from the post office, initial location, and time of registration.
    Accepts a:
        a. UUID - <string>
        b. Initial location - <long,lat>
        c. Time of registration - <Unix Time>

!["register_pckg" graph](https://cdn.discordapp.com/attachments/1097586755391197307/1110978762343977101/20230524_110950.jpg)

2. "vehicle_location_update" - The process used to request and recieve precice location updates from delivery trucks. 
    Accepts a:
        a. Vehicle UUID - <string>
        b. Latitude - <long>
        c. Longitude - <long>

    Gives back:
        a. "OK"

!["Vehicle_location_update" graph](https://cdn.discordapp.com/attachments/1097586755391197307/1110984135553581148/20230524_113234.jpg)

5. "request_location" - Return the coordinates of the truck, at the request of the individual.

    Accepts a:
        a. Package UUID - <string>

    Gives back:
        a. Latitude - <long>
        b. Longitude - <long>
        c. ETA - <time>
        d. History - <list>

!["request_location" graph](https://cdn.discordapp.com/attachments/1097586755391197307/1110994804931829862/20230524_121330_1.jpg)

6. "put_on_vehicle" - Transition a package from a distribution center to a delivery vehicle.
    Accepts a:
        a. Vehicle UUId - <string>
        b. Latitude - <long>
        c. Longitude - <long>
        
    Gives back:
        a. "OK"

!["vehicle_location_update" graph](https://cdn.discordapp.com/attachments/1097586755391197307/1110984135553581148/20230524_113234.jpg)

3. "enter_center" - Transition a package's marked location from a vehicle to a distrobution center.
    Accepts a:
        a. Package UUID - <string>
        b. Center UUID - <string>
        c. Time - <Unix Time>

    Gives back:
        a. "OK"

!["enter_center" graph](https://cdn.discordapp.com/attachments/1097586755391197307/1110990442037788752/20230524_115705.jpg)

4. "mark_delivered" - Mark a package as "delivered" at it's final destination.
    Accepts a:
        a. Package UUID - <string>
        b. Time - <Unix Time>

    Gives back:
        a. "OK"

!["mark_delivered" graph](https://cdn.discordapp.com/attachments/1097586755391197307/1110992725542703244/20230524_120514.jpg)


