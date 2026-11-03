;;;; expected.sexp -- expected values of the Chinook database
;;;;
;;;; Taken from the test suite of the Chinook project, file
;;;; ChinookDatabase.Test/DatabaseTests/ChinookSqliteFixture.cs, commit
;;;; 7f67772503d71ba90f19283c38e93923addb43fa (2025-10-05), Chinook 1.4.5,
;;;; (c) Luis Rocha, MIT license (LICENSE-Chinook.md). The C# suite compares
;;;; ToString() values; here they are typed: integers, decimals as rationals,
;;;; dates as (:date year month day), NULL as NIL.
;;;;
;;;; (:count TABLE N)                    TABLE has N rows
;;;; (:last TABLE (ORDER-BY...) COLS...) last row in that order has these values
;;;; (:row TABLE (KEY VALUE) COLS...)    the row with KEY = VALUE has these values
;;;;
;;;; Names as in the CamelCase schemas (SQLite, MySQL, SQL Server); the tests
;;;; translate them for the snake_case schema of PostgreSQL.

(:count "Genre" 25)
(:last "Genre" ("GenreId")
   ("GenreId" 25) ("Name" "Opera"))
(:count "MediaType" 5)
(:last "MediaType" ("MediaTypeId")
   ("MediaTypeId" 5) ("Name" "AAC audio file"))
(:count "Artist" 275)
(:last "Artist" ("ArtistId")
   ("ArtistId" 275) ("Name" "Philip Glass Ensemble"))
(:count "Album" 347)
(:last "Album" ("AlbumId")
   ("AlbumId" 347) ("Title" "Koyaanisqatsi (Soundtrack from the Motion Picture)") ("ArtistId" 275))
(:count "Track" 3503)
(:last "Track" ("TrackId")
   ("TrackId" 3503) ("Name" "Koyaanisqatsi") ("AlbumId" 347) ("MediaTypeId" 2) ("GenreId" 10) ("Composer" "Philip Glass") ("Milliseconds" 206005) ("Bytes" 3305164) ("UnitPrice" 99/100))
(:count "Employee" 8)
(:last "Employee" ("EmployeeId")
   ("EmployeeId" 8) ("LastName" "Callahan") ("FirstName" "Laura") ("Title" "IT Staff") ("ReportsTo" 6) ("BirthDate" (:date 1968 1 9)) ("HireDate" (:date 2004 3 4)) ("Address" "923 7 ST NW") ("City" "Lethbridge") ("State" "AB") ("Country" "Canada") ("PostalCode" "T1H 1Y8") ("Phone" "+1 (403) 467-3351") ("Fax" "+1 (403) 467-8772") ("Email" "laura@chinookcorp.com"))
(:count "Customer" 59)
(:last "Customer" ("CustomerId")
   ("CustomerId" 59) ("FirstName" "Puja") ("LastName" "Srivastava") ("Company" nil) ("Address" "3,Raj Bhavan Road") ("City" "Bangalore") ("State" nil) ("Country" "India") ("PostalCode" "560001") ("Phone" "+91 080 22289999") ("Fax" nil) ("Email" "puja_srivastava@yahoo.in") ("SupportRepId" 3))
(:count "Invoice" 412)
(:last "Invoice" ("InvoiceId")
   ("InvoiceId" 412) ("CustomerId" 58) ("InvoiceDate" (:date 2025 12 22)) ("BillingAddress" "12,Community Centre") ("BillingCity" "Delhi") ("BillingState" nil) ("BillingCountry" "India") ("BillingPostalCode" "110017") ("Total" 199/100))
(:count "InvoiceLine" 2240)
(:last "InvoiceLine" ("InvoiceLineId")
   ("InvoiceLineId" 2240) ("InvoiceId" 412) ("TrackId" 3177) ("UnitPrice" 199/100) ("Quantity" 1))
(:count "Playlist" 18)
(:last "Playlist" ("PlaylistId")
   ("PlaylistId" 18) ("Name" "On-The-Go 1"))
(:count "PlaylistTrack" 8715)
(:last "PlaylistTrack" ("PlaylistId" "TrackId")
   ("PlaylistId" 18) ("TrackId" 597))
(:row "Customer" ("CustomerId" 1)
   ("CustomerId" 1) ("FirstName" "Luís") ("LastName" "Gonçalves") ("Company" "Embraer - Empresa Brasileira de Aeronáutica S.A.") ("Address" "Av. Brigadeiro Faria Lima, 2170") ("City" "São José dos Campos") ("State" "SP") ("Country" "Brazil") ("PostalCode" "12227-000") ("Phone" "+55 (12) 3923-5555") ("Fax" "+55 (12) 3923-5566") ("Email" "luisg@embraer.com.br") ("SupportRepId" 3))
(:row "Customer" ("CustomerId" 2)
   ("CustomerId" 2) ("FirstName" "Leonie") ("LastName" "Köhler") ("Company" nil) ("Address" "Theodor-Heuss-Straße 34") ("City" "Stuttgart") ("State" nil) ("Country" "Germany") ("PostalCode" "70174") ("Phone" "+49 0711 2842222") ("Fax" nil) ("Email" "leonekohler@surfeu.de") ("SupportRepId" 5))
(:row "Customer" ("CustomerId" 3)
   ("CustomerId" 3) ("FirstName" "François") ("LastName" "Tremblay") ("Company" nil) ("Address" "1498 rue Bélanger") ("City" "Montréal") ("State" "QC") ("Country" "Canada") ("PostalCode" "H2G 1A7") ("Phone" "+1 (514) 721-4711") ("Fax" nil) ("Email" "ftremblay@gmail.com") ("SupportRepId" 3))
(:row "Customer" ("CustomerId" 4)
   ("CustomerId" 4) ("FirstName" "Bjørn") ("LastName" "Hansen") ("Company" nil) ("Address" "Ullevålsveien 14") ("City" "Oslo") ("State" nil) ("Country" "Norway") ("PostalCode" "0171") ("Phone" "+47 22 44 22 22") ("Fax" nil) ("Email" "bjorn.hansen@yahoo.no") ("SupportRepId" 4))
(:row "Customer" ("CustomerId" 5)
   ("CustomerId" 5) ("FirstName" "František") ("LastName" "Wichterlová") ("Company" "JetBrains s.r.o.") ("Address" "Klanova 9/506") ("City" "Prague") ("State" nil) ("Country" "Czech Republic") ("PostalCode" "14700") ("Phone" "+420 2 4172 5555") ("Fax" "+420 2 4172 5555") ("Email" "frantisekw@jetbrains.com") ("SupportRepId" 4))
(:row "Customer" ("CustomerId" 6)
   ("CustomerId" 6) ("FirstName" "Helena") ("LastName" "Holý") ("Company" nil) ("Address" "Rilská 3174/6") ("City" "Prague") ("State" nil) ("Country" "Czech Republic") ("PostalCode" "14300") ("Phone" "+420 2 4177 0449") ("Fax" nil) ("Email" "hholy@gmail.com") ("SupportRepId" 5))
(:row "Customer" ("CustomerId" 7)
   ("CustomerId" 7) ("FirstName" "Astrid") ("LastName" "Gruber") ("Company" nil) ("Address" "Rotenturmstraße 4, 1010 Innere Stadt") ("City" "Vienne") ("State" nil) ("Country" "Austria") ("PostalCode" "1010") ("Phone" "+43 01 5134505") ("Fax" nil) ("Email" "astrid.gruber@apple.at") ("SupportRepId" 5))
(:row "Customer" ("CustomerId" 8)
   ("CustomerId" 8) ("FirstName" "Daan") ("LastName" "Peeters") ("Company" nil) ("Address" "Grétrystraat 63") ("City" "Brussels") ("State" nil) ("Country" "Belgium") ("PostalCode" "1000") ("Phone" "+32 02 219 03 03") ("Fax" nil) ("Email" "daan_peeters@apple.be") ("SupportRepId" 4))
(:row "Customer" ("CustomerId" 9)
   ("CustomerId" 9) ("FirstName" "Kara") ("LastName" "Nielsen") ("Company" nil) ("Address" "Sønder Boulevard 51") ("City" "Copenhagen") ("State" nil) ("Country" "Denmark") ("PostalCode" "1720") ("Phone" "+453 3331 9991") ("Fax" nil) ("Email" "kara.nielsen@jubii.dk") ("SupportRepId" 4))
(:row "Customer" ("CustomerId" 10)
   ("CustomerId" 10) ("FirstName" "Eduardo") ("LastName" "Martins") ("Company" "Woodstock Discos") ("Address" "Rua Dr. Falcão Filho, 155") ("City" "São Paulo") ("State" "SP") ("Country" "Brazil") ("PostalCode" "01007-010") ("Phone" "+55 (11) 3033-5446") ("Fax" "+55 (11) 3033-4564") ("Email" "eduardo@woodstock.com.br") ("SupportRepId" 4))
(:row "Customer" ("CustomerId" 11)
   ("CustomerId" 11) ("FirstName" "Alexandre") ("LastName" "Rocha") ("Company" "Banco do Brasil S.A.") ("Address" "Av. Paulista, 2022") ("City" "São Paulo") ("State" "SP") ("Country" "Brazil") ("PostalCode" "01310-200") ("Phone" "+55 (11) 3055-3278") ("Fax" "+55 (11) 3055-8131") ("Email" "alero@uol.com.br") ("SupportRepId" 5))
