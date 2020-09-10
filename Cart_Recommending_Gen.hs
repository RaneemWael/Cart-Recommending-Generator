import System.Random
import System.IO.Unsafe

users = ["user1", "user2", "user3", "user4"]
items = ["item1", "item2", "item3", "item4", "item5", "item6"]
purchasesHistory =  [
                        ("user1", [["item1", "item2", "item3"], ["item1", "item2", "item4"]]),
                        ("user2", [["item2", "item5"], ["item4", "item5"]]),
                        ("user3", [["item3", "item2"]]),
                        ("user4", [])
                    ]

randomZeroToX :: Int -> Int
randomZeroToX x= unsafePerformIO (getStdRandom (randomR (0, x)))


--get list of s elhowa eluser purchases all concatenated from purchasesHistory
findList s = findList1 s 0
findList1 s x |(checkUser s (purchasesHistory !! x)) == True = (makePurchasesOneList 0 (purchasesHistory !! x))
			  | otherwise = (findList1 s (x+1))
		
		
--get (user, list of purchases)	of s from purchasesHistory	
findIndex s = findIndex1 s 0
findIndex1 s x |(checkUser s (purchasesHistory !! x)) == True = (purchasesHistory !! x)
			   | otherwise = (findIndex1 s (x+1))


--check if index we're at is that of user s
checkUser s (user, list) | user == s = True
						 | otherwise = False


--check if cart of current index is empty						
checkCartNotEmpty (user, list) | list == [] = False
							   | otherwise = True

--seperates list of lists of purchases from user							  
seperate (user, list) = list

--gets purchases list of lists of user x
getPur x = seperate (findIndex x)

--count s occured kam marra fi list
count s [] = 0
count s (x:xs) | (x==s) = (1 + (count s xs)) 
			   | otherwise = (count s xs)


--counts in how many lists c is present in the list of lists		
repeated2 c [] = 0		
repeated2 c (x:xs) | elem c x == True = 1 + (repeated2 c xs)
				   | otherwise = (repeated2 c xs)
				
				
--get length of the all lists from list of lists that a occurs in				  
occursInLen a [] = 0
occursInLen a (x:xs) | elem a x == True = (length x) + occursInLen a xs
					 | otherwise = occursInLen a xs
				

--remove duplicates from a list of lists
removeDuplicates [] = []
removeDuplicates x | elem (last x) (init x) == True = removeDuplicates (init x)
				   | otherwise = removeDuplicates (init x) ++ [last x]
					
					
--makes purchases list of lists of user into one list (y starts at 0)
makePurchasesOneList y (user, list) | y < (length list) = (list !! y) ++ (makePurchasesOneList (y+1) (user, list))
									| otherwise = []


--makes list of lists one list
makeOneList list = makeOneList1 0 list
makeOneList1 y list | y < (length list) = (list !! y) ++ (makeOneList1 (y+1) list)
				    | otherwise = []


createEmptyFreqList :: [a] -> [(a, [b])]
createEmptyFreqList [] = []
createEmptyFreqList (x:xs) = (x, []) : createEmptyFreqList xs



getAllUsersStats :: [(String, [[String]])] -> [(String, [(String, [(String, Int)])])]
getAllUsersStats [] = []
getAllUsersStats ((user, pur):xs) = getAllUserStats user pur : getAllUsersStats xs
getAllUserStats user pur = (user, getItemsStats items pur) 
getItemsStats [] pur = []
getItemsStats (x:xs) pur = (x, removeDuplicates (makeOneList (getItemStats x pur pur))): getItemsStats xs pur

--gets how many times an item x repeats with each item bought (input pur twice)
getItemStats x [] list = []
getItemStats x (y:ys) list | elem x y == True = getList x y list : getItemStats x ys list
						   | otherwise = getItemStats x ys list	 						   
getList x [] list = []
getList x (y:ys) list | x/=y = (y, (occurTogether x y list)) : getList x ys list
					  | otherwise = getList x ys list
					  
--gets how many times two items occur together					  
occurTogether x y [] = 0
occurTogether x y (z:zs) | elem x z && elem y z = 1+occurTogether x y zs
						 | otherwise = occurTogether x y zs
	


freqListItems :: String -> [(String, Int)]
freqListItems s = removeDuplicates (freqListItems1 (findList s) (findList s) (findIndex s))
freqListItems1 listAll [] (user, pur) = []
freqListItems1 listAll (x:xs) (user, pur) = (x, (occursInLen x pur)-(count x listAll)) : (freqListItems1 listAll xs (user, pur))
										  
				
				
freqListCart:: String ->[String] -> [(String, Int)]										  
freqListCart s list = add (makeOneList (freqListCart1 s list))
freqListCart1 s [] = []
freqListCart1 s (x:xs) = removeDuplicates (makeOneList (getItemStats x (getPur s) (getPur s))) : freqListCart1 s xs

--adds the freq of items (takes as input any number of concatenated lists)					 
add list = removeZeros (add1 items list)
add1 [] list = []
add1 (x:xs) list = add2 x list 0 : add1 xs list
add2 i [] c = (i, c)
add2 i ((i1, c1):ys) c | i==i1 = add2 i ys (c+c1)
					   | otherwise = add2 i ys c
					   
--removes items with freq=0					   
removeZeros [] = []					   
removeZeros ((i, c):xs) | c==0 = removeZeros xs
						| otherwise = (i, c) : removeZeros xs



freqListCartAndItems:: String -> [String] -> [(String, Int)]
freqListCartAndItems user list = add ((freqListCart user list) ++ (freqListItems user))



purchasesIntersection ((item, listItems):xs) [] = []
purchasesIntersection ((item, listItems):xs) ((user, freqItems):ys) 
	= purchasesIntersectionUser ((item, listItems):xs) freqItems : purchasesIntersection ((item, listItems):xs) ys

purchasesIntersectionUser [] [] = []
purchasesIntersectionUser ((i1, list1):xs) ((i2, list2):ys)	
	| list1==[] || list2==[] = purchasesIntersectionUser xs ys
	| otherwise = (i1, add (list1++list2)) : purchasesIntersectionUser xs ys
	


freqListUsers:: String -> [(String, Int)]
freqListUsers user = add (removeFirst (makeOneList (purchasesIntersection (findUserStats user) (restUserStats user))))

--gets stats of user without his name
findUserStats user = seperate (getAllUserStats user (seperate (findIndex user)))

--gets stats of all other users except user
restUserStats user = restUserStats1 user users
restUserStats1 user [] = []
restUserStats1 user (x:xs) | x/=user = getAllUserStats x (seperate (findIndex x)) : restUserStats1 user xs
						   | otherwise = restUserStats1 user xs

--removes name of item from beginning of each freq
removeFirst [] = []
removeFirst ((i1, list):xs) = list ++ removeFirst xs



recommendEmptyCart :: String -> String
recommendEmptyCart user | freqListItems user == [] = ""
   					    | otherwise = (genList (freqListItems user)) !! (randomZeroToX ((length (genList (freqListItems user))) - 1)) 

--generates list of items with their frequencies
genList [] = []
genList ((i1, c):xs) | c==0 = genList xs
					 | otherwise = i1 : genList ((i1, c-1):xs)

recommendBasedOnItemsInCart :: String -> [String] -> String
recommendBasedOnItemsInCart user cart | freqListCartAndItems user cart == [] = ""
									  | otherwise = (genList (freqListCartAndItems user cart)) !! (randomZeroToX ((length (genList (freqListCartAndItems user cart))) - 1)) 
									  
									  
									  
recommendBasedOnUsers :: String -> String
recommendBasedOnUsers user | freqListUsers user == [] = ""
						   | otherwise = (genList (freqListUsers user)) !! (randomZeroToX ((length (genList (freqListUsers user))) - 1)) 
						   
						   
recommend :: String -> [String] -> String						   
recommend user cart | recommendBasedOnItemsInCart user cart == [] && recommendBasedOnUsers user == [] = items !! (randomZeroToX ((length (items)) - 1))
					| otherwise = [recommendBasedOnItemsInCart user cart, recommendBasedOnUsers user] !! (randomZeroToX 1)


