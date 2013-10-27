XPTemplate priority=personal

XPT gr " gradient creator
background-color: `start^;
background-image: -webkit-gradient(linear, left top, left bottom, from(`start^), to(`stop^));
background-image: -webkit-linear-gradient(top, `start^, `stop^);
background-image: -moz-linear-gradient(top, `start^, `stop^);
background-image: -o-linear-gradient(top, `start^, `stop^);
background-image: -ms-linear-gradient(top, `start^, `stop^);
background-image: linear-gradient(top, `start^, `stop^);
filter: progid:DXImageTransform.Microsoft.gradient(startColorStr='`start^', EndColorStr='`stop^');

XPT r " border-radius
-moz-border-radius: `radius^;
-webkit-border-radius: `radius^;
border-radius: `radius^;

XPT ! " !important
!important
