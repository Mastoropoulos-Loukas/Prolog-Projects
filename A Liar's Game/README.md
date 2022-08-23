Υπάρχει μία παρέα από φίλους, κάποιοι από τους οποίους λένε πάντα ψέματα, ενώ οι υπόλοιποι λένε πάντα την αλήθεια. Όλοι τους γνωρίζουν για τον καθένα άλλο, αν είναι ψεύτης ή όχι. Κάποια ημέρα, για να περάσουν την ώρα τους, παίζουν το εξής παιγνίδι. Το κάθε άτομο κάνει μία δήλωση της μορφής “στην παρέα υπάρχουν τουλάχιστον K ψεύτες”. Για παράδειγμα, έστω ότι υπάρχουν 5 άτομα και οι δηλώσεις τους είναι οι εξής:<br>
* Ο 1ος δηλώνει: “Υπάρχουν τουλάχιστον 3 ψεύτες μεταξύ μας”
* Ο 2ος δηλώνει: “Υπάρχουν τουλάχιστον 2 ψεύτες μεταξύ μας”
* Ο 3ος δηλώνει: “Υπάρχει τουλάχιστον 1 ψεύτης μεταξύ μας”
* Ο 4ος δηλώνει: “Υπάρχουν τουλάχιστον 4 ψεύτες μεταξύ μας”
* Ο 5ος δηλώνει: “Υπάρχουν τουλάχιστον 2 ψεύτες μεταξύ μας” <br>

Σχετικά εύκολα μπορούμε να διαπιστώσουμε ότι με βάση τα παραπάνω δεδομένα, στην παρέα πρέπει το 1ο και το 4ο άτομο να είναι ψεύτες και μόνο αυτοί. <br>
To κατηγόρημα liars/2, όταν καλείται με πρώτο όρισμα τη λίστα των αριθμών που δηλώνονται από κάθε άτομο ως ελάχιστο πλήθος ψευτών στην παρέα, επιστρέφει στο δεύτερο όρισμα μία λίστα που να δείχνει τι είναι το κάθε άτομο της παρέας, ψεύτης ή όχι, μέσω κατάλληλης τιμής, 1 ή 0. Κάποια παραδείγματα εκτέλεσης είναι τα εξής: <br> <br>
?- liars([3, 2, 1, 4, 2], Liars). <br>
Liars = [1, 0, 0, 1, 0] <br>  <br>
?- liars([9, 1, 7, 1, 8, 3, 8, 9, 1, 3], Liars). <br>
Liars = [1, 0, 1, 0, 1, 0, 1, 1, 0, 0] <br> <br>

Για την δημιουργία μεγαλύτερων εισόδων γίνεται χρήση του κατηγορήματος genrand/2 ως εξής: <br> <br>
?- seed(100), genrand(100, C), liars(C, Liars).<br>
C = [31, 74, 62, 82, 60, 86, 19, 22, 62, 35, 53, 32, 15, 90,
76, 18, 69, 50, 82, ...]<br>
Liars = [0, 1, 1, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0,
1, ...]