[%%cenum
   
type ftype = 
  | Syn
    
  | Ack
  | Nack

  | Stream
  | Rst_Stream

  | Ping
  | Pong


  | Go_Away
    
  | Shutdown
  | Shutdown_ack
 
   


[@@uint8_t]

]






[%%cstruct

type header = {

  cid: uint64_t;
  ftype: uint8_t;
  seq_no: uint64_t;
  
  timestamp: uint64_t;
  len: uint16_t
} [@@big_endian]


]





[%%cstruct

type ack = {
  seq_no: uint64_t;
  rtt: uint64_t;
} [@@big_endian]

]



[%%cstruct

type stream_t = {

  sid: uint32_t;
  fin: uint8_t;
  
  offset: uint64_t;
  len: uint16_t;
} [@@big_endian]


]




[%%cstruct
   
  type rst_stream = {
    sid: uint32_t; code: uint8_t; offset: uint64_t
  } [@@big_endian]
  
]













module Header = struct
  type t = {

    cid: int64;
    ftype: int;
    seq_no: int64;

    timestamp: int64;
    len: int
  }


  let write buf t =
    set_header_len buf t.len;
    set_header_cid buf t.cid;
    set_header_ftype buf t.ftype;

    set_header_timestamp buf t.timestamp;
    set_header_seq_no buf t.seq_no;;
    

  let read buf =


    try 
      let cid = get_header_cid buf in
      let len = get_header_ftype buf in
      let seq_no = get_header_seq_no buf in

      let timestamp = get_header_timestamp buf in
      let ftype = get_header_ftype buf in

      Ok {cid; len; seq_no; timestamp; ftype}
    

    with _ ->
      Error "unable to parse headers"


  
  let size t =
    sizeof_header


  

  

  
        
  
end














module Payload = struct


 
  type t =
    | SYN of {seq_no: int64; window: int64}
    | ACK of {seq_no: int64; rtt: int64}

    | NACK of (int64 list)
    | STREAM of {
      sid: int32; fin: bool;
      offset: int64; len: int;
      payload: Cstruct.t 
    }

    | RST_STREAM of {sid: int32; code: int; off: int64}
    | PING
    | PONG

    | GO_AWAY of int64

    | SHUTDOWN
    | SHUTDOWN_ACK







  



  let encode_nack_list buf t =

    let buf1 =
      let _ = Cstruct.set_uint8 buf 0 (List.length t) in
      Cstruct.shift buf 1
    in
    

    let _ = List.fold_left ( fun acc x ->
        Cstruct.BE.set_uint64 acc 0 x;
        Cstruct.shift acc 8
      ) buf1 t
    in

    ()








  


  let decode_nack_list buf =
    let size = Cstruct.get_uint8 buf 0 in

    let rec aux i buf nack_list =
      if i > 0 then
        let i1 = i - 1 in
        let seq_no = Cstruct.BE.get_uint64 buf 0 in
        let buf1 = Cstruct.shift buf 8 in
        aux i1 buf1 (nack_list @ [seq_no])
          
      else
        nack_list 
    in

    let buffer = Cstruct.shift buf 1 in 
    aux size buffer []




  

  let int_of_bool b =
    if true then 0
    else 1





  let bool_of_int i =
    if i = 0 then true
    else false 





  


  let write buf t =
    match t with
    | SYN {seq_no; window} ->
      Cstruct.BE.set_uint64 buf 0 seq_no;
      Cstruct.BE.set_uint64 buf 8 window;

    | ACK {seq_no; rtt} ->
      set_ack_seq_no buf seq_no;
      set_ack_rtt buf rtt

    | NACK l ->
      encode_nack_list buf l

    | STREAM {sid; fin; offset; len; payload} ->
      set_stream_t_len buf len;
      set_stream_t_sid buf sid;
      set_stream_t_offset buf offset;
      set_stream_t_fin buf (int_of_bool fin);

      let len = sizeof_stream_t in
      let (off, size) = len, len + (Cstruct.len payload) in 
      Cstruct.blit payload 0 buf off size;

    | PING ->
      ()

    | PONG ->
      ()

    | GO_AWAY time_out ->
      Cstruct.BE.set_uint64 buf 0 time_out;


    | RST_STREAM {sid; code; off} ->
      set_rst_stream_offset buf off;
      set_rst_stream_code buf code;
      set_rst_stream_sid buf sid;
      
    | SHUTDOWN -> ()
    | SHUTDOWN_ACK -> ()












  

  let read hdrs buf =
    let open Header in 
    let ftype = int_to_ftype hdrs.ftype in


    try 
      match ftype with

      | Some Ack ->
        let seq_no = get_ack_seq_no buf in
        let rtt = get_ack_rtt buf in 
        Ok ( ACK {seq_no; rtt} )


      | Some Syn ->
        let window = Cstruct.BE.get_uint64 buf 0 in
        let seq_no =  Cstruct.BE.get_uint64 buf 8 in
        Ok ( SYN {window; seq_no} )

      | Some Nack ->
        Ok ( NACK (decode_nack_list buf) ) 

      | Some Stream ->
        let dlen = sizeof_stream_t in
        let sid = get_stream_t_sid buf in
        let fin = get_stream_t_fin buf |> bool_of_int in

        let offset = get_stream_t_offset buf in
        let len = get_stream_t_len buf in

        let payload = Cstruct.shift buf dlen in

        Ok ( STREAM { len; offset; fin; sid; payload} )



      | Some Ping -> Ok PING

      | Some Pong -> Ok PONG

      | Some Go_Away ->
        let tmout = Cstruct.BE.get_uint64 buf 0 in
        Ok (GO_AWAY tmout)

      | Some Shutdown -> Ok SHUTDOWN

      | Some Shutdown_ack -> Ok SHUTDOWN_ACK

      | _ -> Error "Unable to decode payload"


    with _ ->
      Error "Unable to decode payload"






  let size =
    function
    | SYN _ -> 16
    | ACK _ -> 16
    | NACK l -> 1 + ( (List.length l) * 8 )
    | STREAM frame ->
      sizeof_stream_t + (Cstruct.len frame.payload)

    | RST_STREAM _ -> sizeof_rst_stream
    | PING -> 0
    | PONG -> 0
      
    | GO_AWAY _ -> 8
    | SHUTDOWN -> 0
    | SHUTDOWN_ACK -> 0


  

  
    
end 
