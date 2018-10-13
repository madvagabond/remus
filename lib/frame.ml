
[%%cenum
   
type ftype = 
  | SYN
    
  | ACK
  | NACK

  | STREAM
  | RST_STREAM

  | PING
  | PONG


  | GO_AWAY
    
  | SHUTDOWN
  | SHUTDOWN_ACK
 
   

[@@uint8_t]

]





[%%cstruct

   
type header = {

  connection_id: uint64_t;
  ftype: uint8_t;
  seq_no: uint32_t;
  
  timestamp: uint64_t;
  len: uint16_t
}[@@big_endian]


]





[%%cstruct
   
type ack = {
  ack_no: uint32_t;
  rtt: uint64_t;
} [@@big_endian]

]


type stream = {

  sid: Cstruct.uint32;
  fin: bool;
  
  offset: Cstruct.uint64;
  len: Cstruct.uint16;
  data: Cstruct.t
}


[%%cstruct
   
  type rst_stream = {
    sid: uint32_t; code: uint8_t; offset: uint64_t
  } [@@big_endian]

]




type frame =
  | Syn of int32
  | Ping
  | Pong

  | Ack of Cstruct.t
  | Nack of int32 list

  | Stream of stream
  | Rst_stream of int32
  | Go_away of Cstruct.t

  | Shutdown of int64
  | Shutdown_Ack of int64


  



let get_ftype t = int_to_ftype (get_header_ftype t)


