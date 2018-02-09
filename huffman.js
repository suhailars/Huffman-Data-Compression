// begin to learn
function freq(str){

     var freqs={};
     var len=str.length;
     for( var i=0;i<len;i++) {
         var s = str[i];
          if( freqs[s] == undefined){
                freqs[s]=1; 
                 }
          else  
             freqs[s] += 1; 
              
        }
     return freqs; 
            }

                 
freqs=freq("malayalam");
function sortfreq(freqs){
           letters=[];
           for(var key in freqs )
              letters.push([freqs[key],key]);
           return(letters.sort());
            }


tuples=sortfreq(freqs);
function buildTree(tuples){
    
          while ((tuples.length) > 1){
           var leastTwo=tuples.slice(0,2);
           var theRest=tuples.slice(2,tuples.length);
           var combFreq=leastTwo[0][0] + leastTwo[1][0]; 
           tuples=theRest;
           var branch=[combFreq,leastTwo];
           tuples.push(branch); 
           tuples.sort();           
            } 
           return tuples[0]  
           }
tree=buildTree(tuples);
function trimTree(tree){
        var l,r;
        var t=[]
        var p = tree[1];
         if (typeof p === 'string'){
             return p;}
         else {
             return (Array(trimTree(p[0]),trimTree(p[1])));
             
            } 
           }

 
var codes={};
function assigncode(node,pat){
          pat= pat || "";
          if (typeof node === 'string'){
              codes[node]=pat; 
                      }
          else{
               assigncode(node[0],pat+"0");
               assigncode(node[1],pat+"1");
             }
           }  
assigncode(trimTree(tree));
function encode(str){
                  var output="";
                  for(var i=0; i < str.length;i++){
                       output= output + codes[str[i]];
                  }
                  return output;
               }
var l=encode("malayalam");        
function decode(tree,str){
          var output=""
          var p=tree;
          for(var i=0;i<str.length;i++){
              if (str[i] == '0'){
                      p=p[0];
                       }
              else{  
                     p=p[1];
                }    
              if(typeof p === 'string'){
                 output=output+p;
                 p=tree;
                }
            }
          return output;
         }
console.log(l," ",decode(trimTree(tree),l));      
                     
                
