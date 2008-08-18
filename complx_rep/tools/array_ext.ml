module ArrayExt = 
  struct
    type 'a t = 
	{size:int;
         array:'a array}

    let create a = 
      {size = 0;
       	 array = Array.create 0 (None:'a option)} 

    
  end
