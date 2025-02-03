procedure Ors is
begin
Rcc.Ahb1enr := Rcc.Ahb1enr      or RCC_Bits.Ahb1enr.Dma2en or
       RCC_Bits.Ahb1enr.Dma1en  or RCC_Bits.Ahb1enr.Gpioaen or
       RCC_Bits.Ahb1enr.Gpioben or RCC_Bits.Ahb1enr.Gpiocen or
       RCC_Bits.Ahb1enr.Gpioden or RCC_Bits.Ahb1enr.Gpioeen or
       RCC_Bits.Ahb1enr.Gpiofen or RCC_Bits.Ahb1enr.Gpiogen or
       RCC_Bits.Ahb1enr.Gpiohen or RCC_Bits.Ahb1enr.Gpioien;
end Ors;
