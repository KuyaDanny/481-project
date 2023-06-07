There are five parts to this project:

1. "register_pckg" - The ability to register a new package in the system, adding a uuid from the post office, initial location, and time of registration.
    Accepts a:
        a. UUID - <string>
        b. Initial location - <long,lat>
        c. Time of registration - <Unix Time>

!["register_pckg" graph](https://cdn.discordapp.com/attachments/1097586755391197307/1110978762343977101/20230524_110950.jpg)

2. "vehicle_location_update" - The process used to request and recieve precice location updates from delivery trucks. 

5. "request_location" - Return the coordinates of the truck, at the request of the individual.

6. "put_on_vehicle" - Transition a package from a distribution center to a delivery vehicle.

!["vehicle_location_update" graph](https://cdn.discordapp.com/attachments/1097586755391197307/1110984135553581148/20230524_113234.jpg)

3. "enter_center" - Transition a package's marked location from a vehicle to a distrobution center.

!["enter_center" graph](https://cdn.discordapp.com/attachments/1097586755391197307/1110990442037788752/20230524_115705.jpg)

4. "mark_delivered" - Mark a package as "delivered" at it's final destination.

!["mark_delivered" graph](https://cdn.discordapp.com/attachments/1097586755391197307/1110992725542703244/20230524_120514.jpg)


