open Eliom_content
open Inhca_data
open Unprime
open Unprime_char
open Unprime_option
open Unprime_string

let th_p s = Html5.F.th [Html5.F.pcdata s]

let request_table : Inhca_data.request Ocsipersist.table =
  Ocsipersist.open_table "requests"

let base_dn =
  List.map (fun s -> Option.get (String.cut_affix "=" s))
    (String.chop_affix "," Inhca_config.subject_base_dn#get)

let main_service =
  Eliom_service.service
    ~path:[]
    ~get_params:Eliom_parameter.unit ()

let keygen_service =
  Eliom_service.service
    ~path:["req"]
    ~get_params:Eliom_parameter.(suffix (string "request_id")) ()

let signing_service =
  Eliom_service.post_service
    ~fallback:keygen_service
    ~post_params:Eliom_parameter.(string "spkac") ()

let main_handler () () =
  Lwt.return Html5.F.(Inhca_tools.F.page ~title:"Inhca" [
    p [pcdata "Nothing to see here."]
  ])

let keygen_handler request_id () =
  lwt req = Ocsipersist.find request_table request_id in
  Lwt.return Html5.F.(Inhca_tools.F.page ~title:"Fetch Certificate" [
    post_form ~service:signing_service
      (fun spkac -> [
	table
	  (tr [th_p "Full name"; td [pcdata req.request_cn]])
	  [tr [th_p "Email"; td [pcdata req.request_email]];
	   tr [th_p "Key strength";
	       td [keygen
		    ~a:[a_name (Eliom_parameter.string_of_param_name spkac)]
		    ()]];
	   tr [td [];
	       td [string_input ~input_type:`Submit ~value:"Submit" ()]] ]
      ])
      request_id
  ])

let signing_handler request_id spkac =
  try
    lwt req = Ocsipersist.find request_table request_id in
    Ocsipersist.remove request_table request_id >>
    let spkac = String.filter (not *< Char.is_space) spkac in
    let spkac_req = ("SPKAC", spkac) :: ("CN", req.request_cn) :: base_dn in
    lwt cert = Inhca_openssl.sign_spkac request_id spkac_req in
    Eliom_registration.File.send ~content_type:"application/x-x509-user-cert"
				 cert
  with Not_found ->
    Inhca_tools.F.send_error ~code:404 "No such certificate request."

let () =
  let open Eliom_registration in
  let content_type = "text/html" in
  Html5.register ~content_type ~service:main_service main_handler;
  Html5.register ~content_type ~service:keygen_service keygen_handler;
  Any.register ~service:signing_service signing_handler
