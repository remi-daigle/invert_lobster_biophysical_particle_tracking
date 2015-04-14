       integer jpk
       parameter (jpk=46)
       real :: gdepw(jpk),gdept(jpk),e3w(jpk),e3t(jpk) 
!
       ppkth=23.56;
       ppacr=9.000
       ppdzmin=6.0
       pphmax=5750
       zkth = ppkth   
       zacr = ppacr
       zdzmin = ppdzmin
       zhmax = pphmax
!
        za1 = ( ppdzmin - pphmax / (jpk-1) ) / ( tanh((1-ppkth)/ppacr) - ppacr/(jpk-1)*  (  log( cosh( (jpk - ppkth) / ppacr) )- log( cosh( ( 1  - ppkth) / ppacr) )  )  );

         za0  = ppdzmin - za1 * tanh( (1-ppkth) / ppacr );

         zsur = - za0 - za1 * ppacr * log( cosh( (1-ppkth) / ppacr )  );

         
         
         
          print*,'jk,gdept(jk),gdepw(jk),e3t(jk),e3w(jk))'
        do jk = 1,jpk
            zw =  jk ;
            zt =  jk  + 0.5;
            gdepw(jk) = ( zsur + za0 * zw + za1 * zacr *log( cosh( (zw-zkth)/zacr ) )  );
            if(jk.eq.1) gdepw(jk) = 0.e0
            gdept(jk) = ( zsur + za0 * zt + za1 * zacr *log( cosh( (zt-zkth)/zacr ) )  );
            e3w  (jk) =          za0      + za1        *tanh(      (zw-zkth)/zacr   );
            e3t  (jk) =          za0      + za1        *tanh(      (zt-zkth)/zacr   );
            print*,jk,gdept(jk),gdepw(jk),e3t(jk),e3w(jk)
        enddo
        end
