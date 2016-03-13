if(Object.create===undefined)Object.create=function(o){function F(){};F.prototype=o;return new F()}
function Fay$$objConcat(a,b){for(var p in b)if(b.hasOwnProperty(p))a[p]=b[p];return a}
function Fay$$_(thunkish,nocache){while(thunkish instanceof Fay$$$){thunkish=thunkish.force(nocache)};return thunkish}
function Fay$$__(){var f=arguments[0];for(var i=1,len=arguments.length;i<len;i++)f=(f instanceof Fay$$$?Fay$$_(f):f)(arguments[i]);return f}
function Fay$$$(value){this.forced=false;this.value=value};Fay$$$.prototype.force=function(nocache){return nocache?this.value():(this.forced?this.value:(this.value=this.value(),this.forced=true,this.value))}
function Fay$$seq(x){return function(y){Fay$$_(x,false);return y}}
function Fay$$seq$36$uncurried(x,y){Fay$$_(x,false);return y}
function Fay$$Monad(value){this.value=value}
function Fay$$then(a){return function(b){return Fay$$bind(a)(function(_){return b})}}
function Fay$$then$36$uncurried(a,b){return Fay$$bind$36$uncurried(a,function(_){return b})}
function Fay$$bind(m){return function(f){return new Fay$$$(function(){var monad=Fay$$_(m,true);return Fay$$_(f)(monad.value)})}}
function Fay$$bind$36$uncurried(m,f){return new Fay$$$(function(){var monad=Fay$$_(m,true);return Fay$$_(f)(monad.value)})}
function Fay$$$_return(a){return new Fay$$Monad(a)}
function Fay$$force(thunk){return function(type){return new Fay$$$(function(){Fay$$_(thunk,type);return new Fay$$Monad(Fay$$unit)})}}
function Fay$$return$36$uncurried(a){return new Fay$$Monad(a)};var Fay$$unit=null
function Fay$$fayToJs(type,fayObj){var base=type[0],args=type[1],jsObj;if(base=="action"){return function(){return Fay$$fayToJs(args[0],Fay$$_(fayObj,true).value)}}else if(base=="function"){return function(){var fayFunc=fayObj,return_type=args[args.length-1],len=args.length;if(len>1){fayFunc=Fay$$_(fayFunc,true);if(args=="automatic_function"){for(var i=0;i<arguments.length;i++)fayFunc=Fay$$fayToJs(["automatic"],Fay$$_(fayFunc(Fay$$jsToFay(["automatic"],arguments[i])),true));return fayFunc};for(var i=0,len=len;i<len-1&&fayFunc instanceof Function;i++)fayFunc=Fay$$_(fayFunc(Fay$$jsToFay(args[i],arguments[i])),true);var return_base=return_type[0],return_args=return_type[1];if(return_base=="action"){return Fay$$fayToJs(return_args[0],fayFunc.value)}else return Fay$$fayToJs(return_type,fayFunc)}else throw new Error("Nullary function?")}}else if(base=="string"){return Fay$$fayToJs_string(fayObj)}else if(base=="list"){var arr=[];fayObj=Fay$$_(fayObj);while(fayObj instanceof Fay$$Cons){arr.push(Fay$$fayToJs(args[0],fayObj.car));fayObj=Fay$$_(fayObj.cdr)};return arr}else if(base=="tuple"){var arr=[];fayObj=Fay$$_(fayObj);var i=0;while(fayObj instanceof Fay$$Cons){arr.push(Fay$$fayToJs(args[i++],fayObj.car));fayObj=Fay$$_(fayObj.cdr)};return arr}else if(base=="defined"){fayObj=Fay$$_(fayObj);return fayObj instanceof Fay.FFI._Undefined?undefined:Fay$$fayToJs(args[0],fayObj.slot1)}else if(base=="nullable"){fayObj=Fay$$_(fayObj);return fayObj instanceof Fay.FFI._Null?null:Fay$$fayToJs(args[0],fayObj.slot1)}else if(base=="double"||base=="int"||base=="bool"){return Fay$$_(fayObj)}else if(base=="ptr"||base=="unknown"){return fayObj}else if(base=="automatic"&&fayObj instanceof Function){return Fay$$fayToJs(["function","automatic_function"],fayObj)}else if(base=="automatic"||base=="user"){fayObj=Fay$$_(fayObj);if(fayObj instanceof Fay$$Cons||fayObj===null){var arr=[];while(fayObj instanceof Fay$$Cons){arr.push(Fay$$fayToJs(["automatic"],fayObj.car));fayObj=Fay$$_(fayObj.cdr)};return arr}else{var fayToJsFun=fayObj&&fayObj.instance&&Fay$$fayToJsHash[fayObj.instance];return fayToJsFun?fayToJsFun(type,type[2],fayObj):fayObj}};throw new Error("Unhandled Fay->JS translation type: "+base)};var Fay$$fayToJsHash={}
function Fay$$fayToJs_string(fayObj){var str="";fayObj=Fay$$_(fayObj);while(fayObj instanceof Fay$$Cons){str+=Fay$$_(fayObj.car);fayObj=Fay$$_(fayObj.cdr)};return str}
function Fay$$jsToFay_string(x){return Fay$$list(x)}
function Fay$$jsToFay_int(x){return x}
function Fay$$jsToFay_double(x){return x}
function Fay$$jsToFay_bool(x){return x}
function Fay$$fayToJs_int(x){return Fay$$_(x)}
function Fay$$fayToJs_double(x){return Fay$$_(x)}
function Fay$$fayToJs_bool(x){return Fay$$_(x)}
function Fay$$jsToFay(type,jsObj){var base=type[0],args=type[1],fayObj;if(base=="action"){return new Fay$$Monad(Fay$$jsToFay(args[0],jsObj))}else if(base=="function"){var returnType=args[args.length-1],funArgs=args.slice(0,-1);if(jsObj.length>0){var makePartial=function(args){return function(arg){var i=args.length,fayArg=Fay$$fayToJs(funArgs[i],arg),newArgs=args.concat([fayArg]);if(newArgs.length==funArgs.length){return new Fay$$$(function(){return Fay$$jsToFay(returnType,jsObj.apply(this,newArgs))})}else return makePartial(newArgs)}};return makePartial([])}else return function(arg){return Fay$$jsToFay(["automatic"],jsObj(Fay$$fayToJs(["automatic"],arg)))}}else if(base=="string"){return Fay$$list(jsObj)}else if(base=="list"){var serializedList=[];for(var i=0,len=jsObj.length;i<len;i++)serializedList.push(Fay$$jsToFay(args[0],jsObj[i]));return Fay$$list(serializedList)}else if(base=="tuple"){var serializedTuple=[];for(var i=0,len=jsObj.length;i<len;i++)serializedTuple.push(Fay$$jsToFay(args[i],jsObj[i]));return Fay$$list(serializedTuple)}else if(base=="defined"){return jsObj===undefined?new Fay.FFI._Undefined():new Fay.FFI._Defined(Fay$$jsToFay(args[0],jsObj))}else if(base=="nullable"){return jsObj===null?new Fay.FFI._Null():new Fay.FFI.Nullable(Fay$$jsToFay(args[0],jsObj))}else if(base=="int"){fayObj=Math.round(jsObj);if(fayObj!==jsObj)throw "Argument "+jsObj+" is not an integer!";return fayObj}else if(base=="double"||base=="bool"||base=="ptr"||base=="unknown"){return jsObj}else if(base=="automatic"&&jsObj instanceof Function){var type=[["automatic"]];for(var i=0;i<jsObj.length;i++)type.push(["automatic"]);return Fay$$jsToFay(["function",type],jsObj)}else if(base=="automatic"||base=="user")if(jsObj&&jsObj['instance']){var jsToFayFun=Fay$$jsToFayHash[jsObj["instance"]];return jsToFayFun?jsToFayFun(type,type[2],jsObj):jsObj}else if(jsObj instanceof Array){var list=null;for(var i=jsObj.length-1;i>=0;i--)list=new Fay$$Cons(Fay$$jsToFay([base],jsObj[i]),list);return list}else return jsObj;throw new Error("Unhandled JS->Fay translation type: "+base)};var Fay$$jsToFayHash={}
function Fay$$Cons(car,cdr){this.car=car;this.cdr=cdr}
function Fay$$list(xs){var out=null;for(var i=xs.length-1;i>=0;i--)out=new Fay$$Cons(xs[i],out);return out}
function Fay$$cons(x){return function(y){return new Fay$$Cons(x,y)}}
function Fay$$index(index,list){for(var i=0;i<index;i++)list=Fay$$_(list.cdr);return list.car}
function Fay$$listLen(list,max){for(var i=0;list!==null&&i<max+1;i++)list=Fay$$_(list.cdr);return i==max}
function Fay$$mult(x){return function(y){return new Fay$$$(function(){return Fay$$_(x)*Fay$$_(y)})}}
function Fay$$mult$36$uncurried(x,y){return new Fay$$$(function(){return Fay$$_(x)*Fay$$_(y)})}
function Fay$$add(x){return function(y){return new Fay$$$(function(){return Fay$$_(x)+Fay$$_(y)})}}
function Fay$$add$36$uncurried(x,y){return new Fay$$$(function(){return Fay$$_(x)+Fay$$_(y)})}
function Fay$$sub(x){return function(y){return new Fay$$$(function(){return Fay$$_(x)-Fay$$_(y)})}}
function Fay$$sub$36$uncurried(x,y){return new Fay$$$(function(){return Fay$$_(x)-Fay$$_(y)})}
function Fay$$divi(x){return function(y){return new Fay$$$(function(){return Fay$$_(x)/Fay$$_(y)})}}
function Fay$$divi$36$uncurried(x,y){return new Fay$$$(function(){return Fay$$_(x)/Fay$$_(y)})}
function Fay$$equal(lit1,lit2){lit1=Fay$$_(lit1);lit2=Fay$$_(lit2);if(lit1===lit2)return true;if(lit1 instanceof Array){if(lit1.length!=lit2.length)return false;for(var len=lit1.length,i=0;i<len;i++)if(!Fay$$equal(lit1[i],lit2[i]))return false;return true}else if(lit1 instanceof Fay$$Cons&&lit2 instanceof Fay$$Cons){do{if(!Fay$$equal(lit1.car,lit2.car))return false;lit1=Fay$$_(lit1.cdr),lit2=Fay$$_(lit2.cdr);if(lit1===null||lit2===null)return lit1===lit2}while(true)}else if(typeof lit1=='object'&&typeof lit2=='object'&&lit1&&lit2&&lit1.instance===lit2.instance){for(var x in lit1)if(!Fay$$equal(lit1[x],lit2[x]))return false;return true}else return false}
function Fay$$eq(x){return function(y){return new Fay$$$(function(){return Fay$$equal(x,y)})}}
function Fay$$eq$36$uncurried(x,y){return new Fay$$$(function(){return Fay$$equal(x,y)})}
function Fay$$neq(x){return function(y){return new Fay$$$(function(){return !(Fay$$equal(x,y))})}}
function Fay$$neq$36$uncurried(x,y){return new Fay$$$(function(){return !(Fay$$equal(x,y))})}
function Fay$$gt(x){return function(y){return new Fay$$$(function(){return Fay$$_(x)>Fay$$_(y)})}}
function Fay$$gt$36$uncurried(x,y){return new Fay$$$(function(){return Fay$$_(x)>Fay$$_(y)})}
function Fay$$lt(x){return function(y){return new Fay$$$(function(){return Fay$$_(x)<Fay$$_(y)})}}
function Fay$$lt$36$uncurried(x,y){return new Fay$$$(function(){return Fay$$_(x)<Fay$$_(y)})}
function Fay$$gte(x){return function(y){return new Fay$$$(function(){return Fay$$_(x)>=Fay$$_(y)})}}
function Fay$$gte$36$uncurried(x,y){return new Fay$$$(function(){return Fay$$_(x)>=Fay$$_(y)})}
function Fay$$lte(x){return function(y){return new Fay$$$(function(){return Fay$$_(x)<=Fay$$_(y)})}}
function Fay$$lte$36$uncurried(x,y){return new Fay$$$(function(){return Fay$$_(x)<=Fay$$_(y)})}
function Fay$$and(x){return function(y){return new Fay$$$(function(){return Fay$$_(x)&&Fay$$_(y)})}}
function Fay$$and$36$uncurried(x,y){return new Fay$$$(function(){return Fay$$_(x)&&Fay$$_(y)})}
function Fay$$or(x){return function(y){return new Fay$$$(function(){return Fay$$_(x)||Fay$$_(y)})}}
function Fay$$or$36$uncurried(x,y){return new Fay$$$(function(){return Fay$$_(x)||Fay$$_(y)})}
function Fay$$Ref(x){this.value=x}
function Fay$$writeRef(ref,x){ref.value=x}
function Fay$$readRef(ref,x){return ref.value}
function Fay$$date(str){return window.Date.parse(str)}