-record(geo,{coordinate,name}).
-type coordinate() :: {x(),y()}.
-type x() :: integer().
-type y() :: integer().
-type geo() :: #geo{coordinate :: coordinate(), name :: string()}.

-record(member_position,{user_name,coordinate}).
-type member_position() :: #member_position{user_name :: string(), coordinate :: coordinate()}.