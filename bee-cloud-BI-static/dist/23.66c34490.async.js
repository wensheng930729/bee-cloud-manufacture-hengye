(window["webpackJsonp"]=window["webpackJsonp"]||[]).push([[23],{"5e/7":function(e,t,a){e.exports={container:"container___1SSjV",header:"header___3M3k1",title:"title___3aLus",btnBox:"btnBox___3OFER",footer:"footer___jTJzL"}},V1nB:function(e,t,a){e.exports={upload:"upload___2l2Ul"}},ebBO:function(e,t,a){"use strict";var l=a("tAuX"),r=a("g09b");Object.defineProperty(t,"__esModule",{value:!0}),t["default"]=void 0;var n=l(a("q1tI"));a("g9YV");var u=r(a("wCAj"));a("IzEo");var d=r(a("bx4M"));a("iQDF");var i=r(a("+eQT"));a("+L6B");var o=r(a("2/Rp"));a("giR+");var c=r(a("fyUT"));a("5NDa");var f=r(a("5rEg"));a("miYZ");var s=r(a("tsqr")),m=r(a("p0pE")),p=r(a("2Taf")),h=r(a("vZ4D")),v=r(a("l4Ni")),y=r(a("ujKo")),g=r(a("MhPg"));a("OaEy");var E=r(a("2fM7"));a("y8nQ");var w,b,_,C=r(a("Vl3Y")),k=r(a("5e/7")),x=r(a("3a4m")),P=r(a("nyfR")),I=a("uk16"),q=r(a("wd/R")),B=C["default"].Item,L=E["default"].Option,N="YYYY-MM-DD",D=(w=C["default"].create(),w((_=function(e){function t(){var e,a;(0,p["default"])(this,t);for(var l=arguments.length,r=new Array(l),n=0;n<l;n++)r[n]=arguments[n];return a=(0,v["default"])(this,(e=(0,y["default"])(t)).call.apply(e,[this].concat(r))),a.state={customers:[],products:[],locations:[],proInfo:[{productId:void 0,productName:void 0,qualityRequirement:void 0,quantity:void 0,unitPrice:void 0}]},a.saveProInfo=function(e){var t=a.state.proInfo;t[0]=(0,m["default"])({},t[0],e),a.setState({proInfo:t})},a.handleOnChange=function(e){var t=e.file,a=e.fileList;return"error"===t.status?(s["default"].error("\u5408\u540c\u6587\u4ef6\u4e0a\u4f20\u5f02\u5e38\uff0c\u8bf7\u91cd\u8bd5"),[]):a.map(function(e){return{uid:e.uid,name:e.name,status:"done",url:e.response?e.response.object.access_url:e.url}})},a.handleAdd=function(){try{_czc1.push(["_trackEvent","\u9500\u552e\u8ba2\u5355","\u65b0\u589e","","",""])}catch(e){}var e=a.props.form.validateFields,t=a.state.proInfo;e(function(e,a){if(!e){var l=[],r=[];if(a.files&&a.files.length&&a.files.forEach(function(e){r.push({fileName:e.name,fileUrl:e.url})}),void 0===t[0].productId||void 0===t[0].productName)return s["default"].warning("\u8bf7\u9009\u62e9\u4ea7\u54c1");l=(0,m["default"])({},a,{files:r,signDate:(0,q["default"])(a.signDate).format(N),deliveryDate:(0,q["default"])(a.deliveryDate).format(N)},t[0]),(0,I.addContractBuy)(l).then(function(e){1===e.code?(s["default"].success("\u65b0\u589e\u6210\u529f"),x["default"].goBack()):s["default"].error(e.message)})}})},a}return(0,g["default"])(t,e),(0,h["default"])(t,[{key:"componentDidMount",value:function(){var e=this;(0,I.getCustomerListByCategory)().then(function(t){1===t.code&&e.setState({customers:t.object})}),(0,I.getProducts)().then(function(t){1===t.code&&e.setState({products:t.object})}),(0,I.getLocationList)().then(function(t){1===t.code&&e.setState({locations:t.object})})}},{key:"render",value:function(){var e=this,t=this.props.form.getFieldDecorator,a=this.state,l=a.customers,r=a.products,s=a.locations,m=[{title:n["default"].createElement("span",null,n["default"].createElement("sup",{style:{color:"red"}},"*"),"\u4ea7\u54c1\u540d\u79f0"),key:"productName",render:function(t,a,l){return n["default"].createElement(E["default"],{style:{width:"80%"},onChange:function(t,a){return e.saveProInfo({productId:t,productName:a.props.children})}},r.map(function(e){return n["default"].createElement(L,{value:e.productId,key:e.productId},e.productName)}))},width:"25%"},{title:"\u8d28\u91cf\u8981\u6c42",key:"qualityRequirement",render:function(t,a,l){return n["default"].createElement(f["default"],{style:{width:"80%"},onChange:function(t){return e.saveProInfo({qualityRequirement:t.target.value})}})},width:"25%"},{title:"\u5408\u540c\u6570\u91cf",key:"quantity",render:function(t,a,l){return n["default"].createElement(c["default"],{style:{width:"80%"},onChange:function(t){return e.saveProInfo({quantity:t})}})},width:"25%"},{title:"\u91c7\u8d2d\u5355\u4ef7",key:"unitPrice",render:function(t,a,l){return n["default"].createElement(c["default"],{style:{width:"80%"},onChange:function(t){return e.saveProInfo({unitPrice:t})}})},width:"25%"}];return n["default"].createElement("div",{className:k["default"].container},n["default"].createElement("div",{className:k["default"].header},n["default"].createElement("span",{className:k["default"].title},"\u65b0\u589e\u9500\u552e\u5408\u540c"),n["default"].createElement("div",{className:k["default"].btnBox},n["default"].createElement(o["default"],{onClick:function(){return x["default"].goBack()}},"\u53d6\u6d88"),n["default"].createElement(o["default"],{onClick:this.handleAdd.bind(this),type:"primary"},"\u63d0\u4ea4"))),n["default"].createElement(d["default"],{title:"\u5408\u540c\u6982\u51b5"},n["default"].createElement(C["default"],null,n["default"].createElement(B,{label:"\u5408\u540c\u7f16\u53f7"},t("contractNum",{rules:[{required:!0,message:"\u8bf7\u8f93\u5165\u5408\u540c\u7f16\u53f7"}]})(n["default"].createElement(f["default"],{style:{width:"75%"},placeholder:"\u8bf7\u8f93\u5165\u5408\u540c\u7f16\u53f7"}))),n["default"].createElement(B,{label:"\u5ba2\u6237"},t("customerId",{rules:[{required:!0,message:"\u8bf7\u9009\u62e9\u5ba2\u6237"}]})(n["default"].createElement(E["default"],{style:{width:"75%"},placeholder:"\u8bf7\u9009\u62e9\u5ba2\u6237"},l.map(function(e){return n["default"].createElement(L,{value:e.id,key:e.id},e.name)})))),n["default"].createElement(B,{label:"\u7b7e\u8ba2\u65e5\u671f"},t("signDate",{rules:[{required:!0,message:"\u8bf7\u9009\u62e9\u7b7e\u8ba2\u65e5\u671f"}]})(n["default"].createElement(i["default"],{allowClear:!1,format:N,style:{width:"75%"},placeholder:"\u8bf7\u9009\u62e9\u7b7e\u8ba2\u65e5\u671f"}))),n["default"].createElement(B,{label:"\u5408\u540c\u91d1\u989d"},t("amount")(n["default"].createElement(c["default"],{style:{width:"75%",marginRight:18},placeholder:"\u8bf7\u8f93\u5165\u5408\u540c\u91d1\u989d"})),"\u5143"),n["default"].createElement(B,{label:"\u8054\u7cfb\u4eba"},t("linkMan")(n["default"].createElement(f["default"],{style:{width:"75%"},placeholder:"\u8bf7\u8f93\u5165\u8054\u7cfb\u4eba"}))),n["default"].createElement(B,{label:"\u8054\u7cfb\u4eba\u624b\u673a\u53f7"},t("linkPhone")(n["default"].createElement(f["default"],{style:{width:"75%"},placeholder:"\u8bf7\u8f93\u5165\u624b\u673a\u53f7"}))))),n["default"].createElement(d["default"],{title:"\u4ea7\u54c1\u4fe1\u606f"},n["default"].createElement(u["default"],{columns:m,dataSource:[{a:1}],pagination:!1})),n["default"].createElement(d["default"],{title:"\u5408\u540c\u6982\u51b5"},n["default"].createElement(C["default"],null,n["default"].createElement(B,{label:"\u5230\u8fbe\u5730"},t("addressId",{rules:[{required:!0,message:"\u8bf7\u9009\u62e9\u5230\u8fbe\u5730"}]})(n["default"].createElement(E["default"],{style:{width:"75%"},placeholder:"\u8bf7\u9009\u62e9\u5230\u8fbe\u5730"},s.map(function(e){return n["default"].createElement(L,{value:e.id,key:e.id},e.name)})))),n["default"].createElement(B,{label:"\u9500\u552e\u65b9\u5f0f"},t("saleMode",{rules:[{required:!0,message:"\u8bf7\u9009\u62e9\u9500\u552e\u65b9\u5f0f"}]})(n["default"].createElement(E["default"],{style:{width:"75%"},placeholder:"\u8bf7\u9009\u62e9\u9500\u552e\u65b9\u5f0f"},n["default"].createElement(L,{value:0},"\u81ea\u63d0"),n["default"].createElement(L,{value:1},"\u5305\u8fd0")))),n["default"].createElement(B,{label:"\u786e\u8ba4\u65b9"},t("confirmPart",{rules:[{required:!0,message:"\u8bf7\u9009\u62e9\u786e\u8ba4\u65b9"}]})(n["default"].createElement(E["default"],{style:{width:"75%"},placeholder:"\u8bf7\u9009\u62e9\u786e\u8ba4\u65b9"},n["default"].createElement(L,{value:0},"\u6211\u65b9\u786e\u8ba4"),n["default"].createElement(L,{value:1},"\u4f9b\u5e94\u5546\u786e\u8ba4")))),n["default"].createElement(B,{label:"\u4ea4\u8d27\u65e5\u671f"},t("deliveryDate",{rules:[{required:!0,message:"\u8bf7\u9009\u62e9\u4ea4\u8d27\u65e5\u671f"}]})(n["default"].createElement(i["default"],{allowClear:!1,format:N,style:{width:"75%"},placeholder:"\u8bf7\u9009\u62e9\u4ea4\u8d27\u65e5\u671f"}))),n["default"].createElement(B,{label:"\u5408\u540c\u6587\u4ef6",style:{display:"flex",alignItems:"center"}},t("files",{valuePropName:"fileList",getValueFromEvent:this.handleOnChange})(n["default"].createElement(P["default"],{accept:"",number:5}))))),n["default"].createElement("div",{className:k["default"].footer},n["default"].createElement("div",{className:k["default"].btnBox},n["default"].createElement(o["default"],{onClick:function(){return x["default"].goBack()}},"\u53d6\u6d88"),n["default"].createElement(o["default"],{onClick:this.handleAdd.bind(this),type:"primary"},"\u63d0\u4ea4"))))}}]),t}(n.Component),b=_))||b);t["default"]=D},nyfR:function(e,t,a){"use strict";var l=a("g09b"),r=a("tAuX");Object.defineProperty(t,"__esModule",{value:!0}),t["default"]=void 0;var n=r(a("q1tI"));a("2qtc");var u=l(a("kLXV"));a("DZo9");var d=l(a("8z0m"));a("Pwec");var i=l(a("CtXQ"));a("miYZ");var o=l(a("tsqr")),c=l(a("2Taf")),f=l(a("vZ4D")),s=l(a("l4Ni")),m=l(a("ujKo")),p=l(a("MhPg")),h=(l(a("17x9")),l(a("V1nB"))),v=a("ABDx"),y=function(e){function t(){var e,a;(0,c["default"])(this,t);for(var l=arguments.length,r=new Array(l),n=0;n<l;n++)r[n]=arguments[n];return a=(0,s["default"])(this,(e=(0,m["default"])(t)).call.apply(e,[this].concat(r))),a.state={previewVisible:!1,previewImage:null},a.handlePreview=function(e){if(-1===e.name.indexOf(".png")&&-1===e.name.indexOf(".jpg")&&-1===e.name.indexOf(".jpeg")&&-1===e.name.indexOf(".gif"))return o["default"].warning("\u6b64\u7c7b\u6587\u4ef6\u65e0\u6cd5\u9884\u89c8");a.setState({previewImage:e.url||e.thumbUrl,previewVisible:!0})},a}return(0,p["default"])(t,e),(0,f["default"])(t,[{key:"render",value:function(){var e=this,t=this.props,a=t.accept,l=t.disabled,r=t.fileList,o=t.number,c=t.beforeUpload,f=t.onChange,s=t.onRemove,m=this.state,p=m.previewVisible,y=m.previewImage,g=r||[],E=n["default"].createElement("div",{className:h["default"].upload},n["default"].createElement(i["default"],{style:{fontSize:20},type:"plus"}),n["default"].createElement("div",null,"\u4e0a\u4f20"));return n["default"].createElement("div",{className:"clearfix"},n["default"].createElement(d["default"],{accept:a,action:v.domain+v.api_factory_prefix+"/file/upload?type=2",disabled:l,fileList:g,beforeUpload:c,onChange:f,onRemove:!0===l?function(){return!1}:s,onPreview:this.handlePreview},g.length<o&&!1===l?E:null),n["default"].createElement(u["default"],{visible:p,footer:null,onCancel:function(){return e.setState({previewImage:null,previewVisible:!1})}},n["default"].createElement("img",{alt:"example",style:{width:"100%"},src:y})))}}]),t}(n.Component);t["default"]=y,y.defaultProps={accept:"image/jpg, image/jpeg, image/png",disabled:!1,fileList:null,number:1,beforeUpload:function(e,t){var a=e.size/1024/1024<10;return a||o["default"].error("\u6587\u4ef6\u5927\u5c0f\u6700\u5927\u652f\u630110M\uff01"),a},onChange:function(){return!1},onRemove:function(){return!0}}},uk16:function(e,t,a){"use strict";var l=a("g09b");Object.defineProperty(t,"__esModule",{value:!0}),t.getCustomerListByCategory=i,t.getProducts=c,t.getLocationList=s,t.addContractBuy=p;var r=l(a("d6i3")),n=l(a("1l/V")),u=l(a("t3Un")),d=a("ABDx");function i(){return o.apply(this,arguments)}function o(){return o=(0,n["default"])(r["default"].mark(function e(){return r["default"].wrap(function(e){while(1)switch(e.prev=e.next){case 0:return e.abrupt("return",(0,u["default"])("".concat(d.api_user_prefix,"/customer/getCustomerListByCategory"),{method:"POST",body:{type:[]}}));case 1:case"end":return e.stop()}},e)})),o.apply(this,arguments)}function c(){return f.apply(this,arguments)}function f(){return f=(0,n["default"])(r["default"].mark(function e(){return r["default"].wrap(function(e){while(1)switch(e.prev=e.next){case 0:return e.abrupt("return",(0,u["default"])("".concat(d.api_factory_prefix,"/buyContractBasic/getProducts"),{method:"GET"}));case 1:case"end":return e.stop()}},e)})),f.apply(this,arguments)}function s(){return m.apply(this,arguments)}function m(){return m=(0,n["default"])(r["default"].mark(function e(){return r["default"].wrap(function(e){while(1)switch(e.prev=e.next){case 0:return e.abrupt("return",(0,u["default"])("".concat(d.api_factory_prefix,"/configLocation/getLocationList"),{method:"GET"}));case 1:case"end":return e.stop()}},e)})),m.apply(this,arguments)}function p(e){return h.apply(this,arguments)}function h(){return h=(0,n["default"])(r["default"].mark(function e(t){return r["default"].wrap(function(e){while(1)switch(e.prev=e.next){case 0:return e.abrupt("return",(0,u["default"])("".concat(d.api_factory_prefix,"/saleContractBasic/addContractBuy"),{method:"POST",body:t}));case 1:case"end":return e.stop()}},e)})),h.apply(this,arguments)}}}]);