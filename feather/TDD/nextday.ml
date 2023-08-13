type day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday


let nextday = function
  | Monday -> Tuesday
  | Tuesday -> Wednesday
  | Wednesday -> Thursday
  | Thursday -> Friday
  | Friday -> Saturday
  | Saturday -> Sunday
  | _ -> failwith "UnImplemented"
