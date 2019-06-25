;Trabalho de Sistemas Embarcados
;Elevador (Acrílico Novo)
;2019/1
;Grupo:
;Brenno Barros Soares
;Dyego Cavalieri Pansieri
;Wallace Kruger


segment code
..start:
		mov 	ax,data
		mov 	ds,ax
		mov 	ax,stack
		mov 	ss,ax
		mov 	sp,stacktop

  ; reconfigurando Interrupção externa

  	xor 	ax, ax
  	mov 	es, ax
  	mov 	ax, [es:int9*4]         ;guarda o que havia na memória (que será usada para guardar a posição da interrupção do teclado) anteriormente
  	mov 	[offset_dos], ax
  	mov 	ax, [es:int9*4+2]
  	mov 	[cs_dos], ax
  	cli
  	mov 	[es:int9*4+2], cs
  	mov  	word [es:int9*4],keyint
  	sti


  ; salvar modo de video

    mov  	ah,0Fh
    int  	10h
    mov  	[modo_anterior],al

  ; alterar modo de video para gráfico 640x480 16 cores

  	mov  	al,12h
   	mov  	ah,0
    int   10h

		mov  	byte[andar_atual],00010000b
		mov		al,0
		mov		dx,318h
		out		dx,al

		mov		al,0
		mov		dx,319h
		out		dx,al


;--------------------------------------------------------------------------------------------------------
; 	Codigo da tela inicial de calibracao
;--------------------------------------------------------------------------------------------------------

		mov		byte[cor],branco_intenso
		call 	telaInicial
		call 	informacoes

Inicio:									;Permaneçe neste loop até a tecla 'espaço' ser solta
	  mov		byte[sentido_elevador],elevador_subindo
	  call  LEDs
   		cmp   byte[tecla],185			;Compara o buffer do teclado quando o espaço for solto
	 	jne   Inicio
	 	mov		byte[sentido_elevador],elevador_descendo
	 	call  LEDs
	 	mov		byte[cor],preto
		call 	telaInicial
		mov		byte[cor],branco_intenso
		call 	telaPrincipal
		call 	escreverDescendo
		call 	escreverFuncionando


;--------------------------------------------------------------------------------------------------------
;		Codigo da tela principal
;--------------------------------------------------------------------------------------------------------

main:
		call	entradasExternas           	;Lê os leds e a ranhura
		mov		cl,byte[sentido_elevador]			;Salva sentido do motor para para restaurar após uma possivel emergencia
		mov		byte[sentido_elevador_anterior],cl

checaEmergencia:

		cmp   byte[sinal_emergencia],nao_emergencia
		je		semEmergencia
		mov		byte[sentido_elevador],elevador_parado
		call	LEDs
		jmp		checaEmergencia

semEmergencia:
		mov		cl,byte[sentido_elevador_anterior]
		mov		byte[sentido_elevador],cl
		call 	tratarRanhura

RecarregaTela:
		cmp		byte[estado_elevador],estado_parado
		jne		naoChecarChamadas
		call	chamadasInternas
		call	chamadasExternas

naoChecarChamadas:
		call	LEDs
		call	AtualizaTela

	  jmp  	main

;--------------------------------------------------------------------------------------------------------
;   Função para atualizar a tela
;--------------------------------------------------------------------------------------------------------

tratarRanhura:

		;Aqui começam os testes das ranhuras,
;também trata uma parte do debounce (precisa sair da ranhura pra reconhecer outra ranhura)

		cmp		byte[ranhura_lida],0 			;verifica se uma ranhura foi lida
		jne		naoRanhura

		cmp		byte[ranhura_anterior],1 			;se sim, verifica se já tratou dessa ranhura
		je		fimTratarRanhura

		cmp		byte[estado_elevador],estado_subindo 		;verifica se o sentido do elevador é subindo
		jne		naoSubindo
		cmp		byte[andar_atual],andar_4 								;se sim, verifica se ele já se encontra no 4 andar
		je		naoSubindo
		mov		al,byte[andar_atual] 											;se não, ele da um SHL no andar_atual para o atualizar
		shl		al,1
		mov		byte[andar_atual],al
		mov		byte[estado_anterior],estado_subindo 		;coloca flag do sentido anterior igual o sentido atual

naoSubindo:
		cmp		byte[estado_elevador],estado_descendo 	;verifica se o sentido do elevador é descendo
		jne		naoDescendo
		cmp		byte[andar_atual],andar_1 								;se sim, verifica se ele já se encontra no 1 andar
		je		naoDescendo
		mov		al,byte[andar_atual] 											;se não, ele da um SHR no andar_atual para o atualizar
		shr		al,1
		mov		byte[andar_atual],al
		mov		byte[estado_anterior],estado_descendo 	;Coloca flag do sentido anterior igual o sentido atual

naoDescendo:
		call	chamadasInternas
		call	chamadasExternas
		mov		byte[ranhura_anterior],1			;diz que essa ranhura ja foi tratada
		jmp 	fimTratarRanhura

naoRanhura:
		mov		byte[ranhura_anterior],0

fimTratarRanhura:
		ret


atualizaSetaInterna1:
		pusha

		mov   byte[cor],branco_intenso
		mov 	cl,[chamadas_internas]
		and   cl,andar_1
		cmp   cl,andar_1
		jne   desenhaSetaInterna1
		mov   byte[cor],vermelho

desenhaSetaInterna1:
		call  SetaInterna1

		popa
		ret

atualizaSetaInterna2:
		pusha

		mov   byte[cor],branco_intenso
		mov 	cl,[chamadas_internas]
		and   cl,andar_2
		cmp   cl,andar_2
		jne   desenhaSetaInterna2
		mov   byte[cor],vermelho

desenhaSetaInterna2:
		call  SetaInterna2

	  popa
		ret

atualizaSetaInterna3:
		pusha

		mov   byte[cor],branco_intenso
		mov 	cl,[chamadas_internas]
		and   cl,andar_3
		cmp  	cl,andar_3
		jne  	desenhaSetaInterna3
		mov   byte[cor],vermelho

desenhaSetaInterna3:
		call  SetaInterna3

		popa
		ret

atualizaSetaInterna4:
		pusha

		mov   byte[cor],branco_intenso
		mov 	cl,[chamadas_internas]
		and   cl,andar_4
		cmp   cl,andar_4
		jne   desenhaSetaInterna4
		mov   byte[cor],vermelho

desenhaSetaInterna4:
		call  SetaInterna4

		popa
		ret



AtualizaTela:
		pusha

		call 	atualizarAndar

		mov   cl,byte[externas_subida]
		mov   bl,byte[externas_subida_anterior]
		and   cl,andar_1
		and   bl,andar_1
		cmp   cl,bl
		je  	naoMoveuExterno1
		mov   byte[cor],branco_intenso
		cmp 	cl,andar_1
		jne 	desenhaSetaExterna1
		mov   byte[cor],vermelho

desenhaSetaExterna1:
		call  SetaExterna1

naoMoveuExterno1:
		mov   cl,byte[externas_subida]
		mov   bl,byte[externas_subida_anterior]
		and   cl,andar_2
		and   bl,andar_2
		cmp   cl,bl
		je  	naoMoveuExterno2subindo
		mov   byte[cor],branco_intenso
		cmp 	cl,andar_2
		jne 	desenhaSetaExterna2subindo
		mov   byte[cor],vermelho

desenhaSetaExterna2subindo:
		call  SetaExterna2subindo

naoMoveuExterno2subindo:
		mov   cl,byte[externas_descida]
		mov   bl,byte[externas_descida_anterior]
		and   cl,andar_2
		and   bl,andar_2
		cmp   cl,bl
		je  	naoMoveuExterno2descendo
		mov   byte[cor],branco_intenso
		cmp 	cl,andar_2
		jne 	desenhaSetaExterna2descendo
		mov   byte[cor],azul

desenhaSetaExterna2descendo:
		call  SetaExterna2descendo

naoMoveuExterno2descendo:
		mov   cl,byte[externas_subida]
		mov   bl,byte[externas_subida_anterior]
		and   cl,andar_3
		and   bl,andar_3
		cmp   cl,bl
		je  	naoMoveuExterno3subindo
		mov   byte[cor],branco_intenso
		cmp 	cl,andar_3
		jne 	desenhaSetaExterna3subindo
		mov   byte[cor],vermelho

desenhaSetaExterna3subindo:
		call  SetaExterna3subindo

naoMoveuExterno3subindo:
		mov   cl,byte[externas_descida]
		mov   bl,byte[externas_descida_anterior]
		and   cl,andar_3
		and   bl,andar_3
		cmp   cl,bl
		je  	naoMoveuExterno3descendo
		mov   byte[cor],branco_intenso
		cmp 	cl,andar_3
		jne 	desenhaSetaExterna3descendo
		mov   byte[cor],azul

desenhaSetaExterna3descendo:
		call  SetaExterna3descendo

naoMoveuExterno3descendo:
		mov   cl,byte[externas_descida]
		mov   bl,byte[externas_descida_anterior]
		and   cl,andar_4
		and   bl,andar_4
		cmp   cl,bl
		je  	naoMoveuExterno4
		mov   byte[cor],branco_intenso
		cmp 	cl,andar_4
		jne 	desenhaSetaExterna4
		mov   byte[cor],azul

desenhaSetaExterna4:
		call  SetaExterna4

naoMoveuExterno4:
		cmp		byte[estado_elevador],estado_parado
		jne 	naoEstadoParado
		call 	escreverParado
		jmp 	fimAtualizaTela

naoEstadoParado:
		cmp		byte[estado_elevador],estado_subindo
		jne 	naoEstadoSubindo
		call 	escreverSubindo
		jmp 	fimAtualizaTela

naoEstadoSubindo:
		call 	escreverDescendo

fimAtualizaTela:
		popa
		ret

sair:
		mov		byte[led_externo],0
		call	atualizaChamadasInternas
		mov  	ah,0										; set video mode
	  mov  	al,[modo_anterior]			; modo anterior
	  int  	10h

    cli
    xor   ax, ax
    mov   es, ax
    mov   ax, [cs_dos]
    mov   [es:int9*4+2], ax
    mov   ax, [offset_dos]
    mov   [es:int9*4], ax
    mov   ah, 4ch
    int   21h



;--------------------------------------------------------------------------------------------------------
; 	 Função para ler a porta 318h para acionamentos
;--------------------------------------------------------------------------------------------------------

LEDs:
		pusha

		mov		cl,byte[sentido_elevador]
		mov		al,byte[led_externo]
		or		al,cl
		mov		dx,318h
		out		dx,al

		popa
		ret

;--------------------------------------------------------------------------------------------------------
;  	Função para ler a porta 319h e detectar os botões e sensor ativados
;--------------------------------------------------------------------------------------------------------

entradasExternas:
		pusha

		mov		dx,0319h

debounce:
		in		al,dx
		mov		bl,al
		mov   cx,3fffh				;faz 3FFFh leituras das portas de entrada

loopDebounce:
		in		al,dx
		cmp		bl,al					; Atualiza os valores lidos, caso haja mudança eles são zerados
		jne 	debounce				; Após varias repetiçoes teremos valores estaveis em bl
		loop	loopDebounce


; --------------------------------------------------------------------------------------------------------
;  Verifica os botões
; --------------------------------------------------------------------------------------------------------

	; subida

		mov 	cl,byte[externas_subida]
		mov 	byte[externas_subida_anterior],cl
		and		al,andar_1_subindo				; Pega apenas o andar que será conferido no momento
		or		byte[externas_subida],al
		or		byte[led_externo],al
		mov		al,bl
		and		al,andar_2_subindo				; Pega apenas o andar que será conferido no momento
		or		byte[led_externo],al
		shr 	al,1
		or		byte[externas_subida],al
		mov		al,bl
		and		al,andar_3_subindo				; Pega apenas o andar que será conferido no momento
		or		byte[led_externo],al
		shr 	al,2
		or		byte[externas_subida],al

	; descida

		mov 	cl,byte[externas_descida]
		mov 	byte[externas_descida_anterior],cl
		mov		al,bl
		and		al,andar_2_descendo				; Pega apenas o andar que será conferido no momento
		or		byte[led_externo],al
		or		byte[externas_descida],al
		mov		al,bl
		and		al,andar_3_descendo				; Pega apenas o andar que será conferido no momento
		or		byte[led_externo],al
		shr		al,1
		or		byte[externas_descida],al
		mov		al,bl
		and		al,andar_4_descendo				; Pega apenas o andar que será conferido no momento
		or		byte[led_externo],al
		shr		al,2
		or		byte[externas_descida],al
									;verificando o sensor de ranhura
		mov		al,bl
		mov		cl,acha_ranhura							; Mascara para indicar se o sensor foi ativado
		and		al,cl											; Pega apenas o andar que será conferido no momento
		mov   byte[ranhura_lida],al										; Atualiza os registradores

		popa
		ret

;--------------------------------------------------------------------------------------------------------
;  Função para atende as chamadas externas
;--------------------------------------------------------------------------------------------------------

chamadasExternas:
		pusha
		mov		dl,00001111b;
		mov		al,byte[externas_subida];
		and		al,dl						          ; isola somente os bits de interesse correspondentes ao andar
		mov		byte[externas_subida],al
		mov		al,byte[externas_descida]
		and		al,dl					           	; isola somente os bits de interesse correspondentes ao andar
		mov		byte[externas_descida],al


		cmp		byte[estado_elevador],estado_descendo	; se nao verifica se tá descendo
		jne		naoDesceExterno2
		jmp		naoDesceExterno3
		nop

naoDesceExterno2:
		jmp		naoDesceExterno

naoDesceExterno3:

		mov		cl,byte[andar_atual];
		mov		al,byte[externas_descida];
		and		al,cl

		cmp		cl,al				;verifica se o andar atual coincide com a chamada externa
		jne		semChamadaExternaDescendo;

		mov		cl,byte[andar_atual]
		mov		al,byte[externas_descida]
		not		cl
		and		al,cl
		mov		byte[externas_descida],al

		mov		cl,byte[andar_atual];

		cmp		cl,andar_4
		jne		andar4
		mov		al,byte[led_externo]
		and		al,11011111b
		mov		byte[led_externo],al

andar4:
		cmp		cl,andar_3
		jne		andar3
		mov		al,byte[led_externo]
		and		al,11110111b
		mov		byte[led_externo],al

andar3:
		cmp		cl,andar_2
		jne		andar2
		mov		al,byte[led_externo]
		and		al,11111101b
		mov		byte[led_externo],al

andar2:
		call	atualizaChamadasInternas
		mov		byte[sentido_elevador],elevador_descendo

semChamadaExternaDescendo:

		mov		cl,byte[externas_descida]

proximaChamadaExternaDescendo:
		shl		cl,1
		and		cl,00001111b;

		cmp		cl,0
		je		naoTemChamadaExternaDescendo

		mov		al,cl
		mov		dl,byte[andar_atual]
		and		al,dl

		cmp		al,dl
		jne		proximaChamadaExternaDescendo
		jmp		comChamada

naoTemChamadaExternaDescendo:
		mov		cl,byte[externas_subida]

proximoAndarExternoSubindo:
		shl		cl,1
		and		cl,00001111b;

		cmp		cl,0
		je		semChamadaExternaSubindo

		mov		al,cl
		mov		dl,byte[andar_atual]
		and		al,dl

		cmp		al,dl
		jne		proximoAndarExternoSubindo
		jmp		comChamada

semChamadaExternaSubindo:

		mov		cl,byte[andar_atual];
		cmp		cl,andar_1
		jne		andar1
		mov		al,byte[led_externo]
		and		al,11111110b
		mov		byte[led_externo],al
		mov		al,byte[externas_subida];
		not		cl
		and		al,cl;
		mov		byte[externas_subida],al
		call	atualizaChamadasInternas
		jmp		pararAndar1

andar1:

		cmp		byte[chamadas_internas],0
		jne		comChamada

pararAndar1:
		mov		byte[sentido_elevador],elevador_parado
		mov		byte[estado_elevador],estado_parado
		mov		byte[estado_anterior],estado_parado

comChamada:
		jmp		fimChamadasInternas

naoDesceExterno:

		cmp		byte[estado_elevador],estado_subindo	; verifica se está subindo
		jne		agoraParaExterno2
		jmp		agoraParaExterno3
		nop

agoraParaExterno2:
		jmp		agoraParaExterno

agoraParaExterno3:

		mov		cl,byte[andar_atual];
		mov		al,byte[externas_subida];
		and		al,cl;

		cmp		cl,al					;verifica se o andar atual coincide com a chamada externa
		jne		naoTemChamadaExternaSubindo;

		mov		cl,byte[andar_atual];
		mov		al,byte[externas_subida];
		not		cl
		and		al,cl;
		mov		byte[externas_subida],al
		mov		cl,byte[andar_atual];

		cmp		cl,andar_1
		jne		andar1naoSobe
		mov		al,byte[led_externo]
		and		al,11111110b
		mov		byte[led_externo],al

andar1naoSobe:
		cmp		cl,andar_2
		jne		andar2naoSobe
		mov		al,byte[led_externo]
		and		al,11111011b
		mov		byte[led_externo],al

andar2naoSobe:
		cmp		cl,andar_3
		jne		andar3naoSobe
		mov		al,byte[led_externo]
		and		al,11101111b
		mov		byte[led_externo],al

andar3naoSobe:

		call	atualizaChamadasInternas
		mov		byte[sentido_elevador],elevador_subindo

naoTemChamadaExternaSubindo:

		mov		cl,byte[externas_subida]

proximoAndarExternoSubindo2:
		shr		cl,1
		and		cl,00001111b;

		cmp		cl,0
		je		semChamadaExternaSubindo2

		mov		al,cl
		mov		dl,byte[andar_atual]
		and		al,dl

		cmp		al,dl
		jne		proximoAndarExternoSubindo2
		jmp		temChamada

semChamadaExternaSubindo2:
		mov		cl,byte[externas_descida]

proximaChamadaExternaDescendo2:
		shr		cl,1
		and		cl,00001111b;

		cmp		cl,0
		je			naoTemChamadaExternaDescendo2

		mov		al,cl
		mov		dl,byte[andar_atual]
		and		al,dl

		cmp		al,dl
		jne		proximaChamadaExternaDescendo2
		jmp		temChamada

naoTemChamadaExternaDescendo2:

		mov		cl,byte[andar_atual];
		cmp		cl,andar_4
		jne		andar4naoSobe
		mov		al,byte[led_externo]
		and		al,11011111b
		mov		byte[led_externo],al
		mov		al,byte[externas_descida];
		not		cl
		and		al,cl;
		mov		byte[externas_descida],al
		call		atualizaChamadasInternas
		jmp		pararAndar4

andar4naoSobe:

		cmp		byte[chamadas_internas],0
		jne		temChamada

pararAndar4:
		mov		byte[sentido_elevador],elevador_parado
		mov		byte[estado_elevador],estado_parado
		mov		byte[estado_anterior],estado_parado

temChamada:
		jmp		fimChamadasInternas

agoraParaExterno:

		cmp		byte[externas_descida],0
		je		nenhumaChamadaExternaDesce

		mov		cl,byte[andar_atual]
		mov		al,byte[externas_descida]
		and		al,cl;

		cmp		cl,al					;verifica se o andar atual coincide com a chamada externa
		jne		naoAndarAtualDescendo


		cmp		cl,andar_2
		jne		andar3paraDesce
		mov		al,byte[led_externo]
		and		al,11111101b
		mov		byte[led_externo],al

andar3paraDesce:
		cmp		cl,andar_3
		jne		andar4paraDesce
		mov		al,byte[led_externo]
		and		al,11110111b
		mov		byte[led_externo],al

andar4paraDesce:
		cmp		cl,andar_4
		jne		continuaDesce
		mov		al,byte[led_externo]
		and		al,11011111b
		mov		byte[led_externo],al

continuaDesce:

		mov		al,byte[externas_descida]
		not		cl
		and		al,cl;
		mov		byte[externas_descida],al
		call	atualizaChamadasInternas
		jmp		fimChamadasInternas

naoAndarAtualDescendo:
		mov		al,byte[andar_atual]
		cmp		byte[externas_descida],al
		ja		temChamadaAcimaDescendo

		mov		byte[sentido_elevador],elevador_descendo
		mov		byte[estado_elevador],estado_descendo
		mov		byte[estado_anterior],estado_descendo
		jmp		fimChamadasInternas

temChamadaAcimaDescendo:
		mov		byte[sentido_elevador],elevador_subindo
		mov		byte[estado_elevador],estado_subindo
		mov		byte[estado_anterior],estado_subindo
		jmp		fimChamadasInternas

nenhumaChamadaExternaDesce:
		cmp		byte[externas_subida],0
		je		fimChamadasInternas

		mov		cl,byte[andar_atual]
		mov		al,byte[externas_subida]
		and		al,cl

		cmp		cl,al					;verifica se o andar atual coincide com a chamada externa
		jne		naoAndarAtualSubindo


		cmp		cl,andar_1
		jne		andar2paraSobe
		mov		al,byte[led_externo]
		and		al,11111110b
		mov		byte[led_externo],al

andar2paraSobe:
		cmp		cl,andar_2
		jne		andar3paraSobe
		mov		al,byte[led_externo]
		and		al,11111011b
		mov		byte[led_externo],al

andar3paraSobe:

		cmp		cl,andar_3
		jne		andar4paraSobe
		mov		al,byte[led_externo]
		and		al,11101111b
		mov		byte[led_externo],al

andar4paraSobe:

		mov		al,byte[externas_subida]
		not		cl
		and		al,cl
		mov		byte[externas_subida],al
		call	atualizaChamadasInternas
		jmp		fimChamadasInternas

naoAndarAtualSubindo:

		mov		al,byte[andar_atual]
		cmp		byte[externas_subida],al
		ja		temChamadaAcimaSubindo

		mov		byte[sentido_elevador],elevador_descendo
		mov		byte[estado_elevador],estado_descendo
		mov		byte[estado_anterior],estado_descendo
		jmp		fimChamadasInternas

temChamadaAcimaSubindo:
		mov		byte[sentido_elevador],elevador_subindo
		mov		byte[estado_elevador],estado_subindo
		mov		byte[estado_anterior],estado_subindo

fimChamadasInternas:

		popa
		ret


;--------------------------------------------------------------------------------------------------------
; 	 Função para atender as chamadas internas
;--------------------------------------------------------------------------------------------------------

chamadasInternas:
		pusha

		mov		dl,00001111b
		mov		cl,byte[andar_atual]
		mov		al,byte[chamadas_internas]
		and		al,dl													; isola somente os bits de interesse correspondentes ao andar
		mov		byte[chamadas_internas],al		;resalva por segurança
		and		al,cl
		cmp		cl,al													;verifica se a chamada interna é a mesma do andar autal
		jne		direçãoChamada
		call	atualizaChamadasInternas

direçãoChamada:

		cmp		byte[estado_elevador],estado_descendo
		jne		semChamadasExternas
		mov		cl,byte[andar_atual]
		mov		al,byte[externas_descida]
		and		al,cl
		cmp		al,cl
		jne		semChamadasExternas
		call	atualizaChamadasInternas

semChamadasExternas:

		cmp		byte[estado_elevador],estado_subindo
		jne		outraDirecao
		mov		cl,byte[andar_atual]
		mov		al,byte[externas_subida]
		and		al,cl
		cmp		al,cl
		jne		outraDirecao
		call	atualizaChamadasInternas

outraDirecao:

		cmp		byte[chamadas_internas],0
		je		semChamadas
		cmp		byte[estado_anterior],estado_parado
		je		agoraDesce

		cmp		byte[estado_elevador],estado_descendo
		jne		naoDesce

semChamadasSubindo:

agoraDesce:

		mov		cl,byte[chamadas_internas]

proximoAndarDescendo:
		shl		cl,1
		and		cl,00001111b

		cmp		cl,0
		je		naoDesce

		mov		al,cl
		mov		dl,byte[andar_atual]
		and		al,dl

		cmp		al,dl
		jne		proximoAndarDescendo
		mov		byte[sentido_elevador],elevador_descendo
		mov		byte[estado_elevador],estado_descendo
		mov		byte[estado_anterior],estado_descendo
		jmp		agoraMove


naoDesce:

		cmp		byte[estado_anterior],estado_parado
		je		agoraSobe

		cmp		byte[estado_elevador],estado_subindo
		jne		agoraPara

semChamadasDescendo:

agoraSobe:

		mov		cl,byte[chamadas_internas]

proximoAndarSubindo:
		shr		cl,1
		and		cl,00001111b

		cmp		cl,0
		je		agoraMove

		mov		al,cl
		mov		dl,byte[andar_atual]
		and		al,dl

		cmp		al,dl
		jne		proximoAndarSubindo

		mov		byte[sentido_elevador],elevador_subindo
		mov		byte[estado_elevador],estado_subindo
		mov		byte[estado_anterior],estado_subindo

		jmp		agoraMove

agoraPara:
		nop
		nop
		nop
		nop

semChamadas:
		cmp		byte[externas_descida],0
		jne		agoraMove

		cmp		byte[externas_subida],0
		jne		agoraMove

		cmp		byte[chamadas_internas],0
		jne		agoraMove

		mov		byte[sentido_elevador],elevador_parado
		mov		byte[estado_elevador],estado_parado
		mov		byte[estado_anterior],estado_parado

agoraMove:
		popa
		ret

;--------------------------------------------------------------------------------------------------------
;  Função para apagar uma chamada
;--------------------------------------------------------------------------------------------------------

atualizaChamadasInternas:
		pusha

		mov		al,byte[andar_atual]
		not		al
		mov		dl,byte[chamadas_internas]
		and		al,dl
		mov		byte[chamadas_internas],al
		mov		al,byte[andar_atual]
		cmp 	al,andar_1
		jne 	naoAtualizaAndar1
		call 	atualizaSetaInterna1

naoAtualizaAndar1:
		cmp 	al,andar_2
		jne 	naoAtualizaAndar2
		call 	atualizaSetaInterna2

naoAtualizaAndar2:
		cmp 	al,andar_3
		jne 	naoAtualizaAndar3
		call 	atualizaSetaInterna3

naoAtualizaAndar3:
		cmp 	al,andar_4
		jne 	naoAtualizaAndar4
		call 	atualizaSetaInterna4

naoAtualizaAndar4:
		mov		cl,byte[sentido_elevador]
		mov		byte[sentido_elevador_anterior],cl
		mov		byte[sentido_elevador],elevador_parado
		call	LEDs
		mov		cl,byte[sentido_elevador_anterior]
		mov		byte[sentido_elevador],cl
		mov		al,00000001b
		mov		dx,319h
		out		dx,al
		call	ctrlLED
		mov		al,00000000b
		mov		dx,319h
		out		dx,al

		popa
		ret


; --------------------------------------------------------------------------------------------------------
;  Função para acender e apagar o LED (da porta 318h)
; --------------------------------------------------------------------------------------------------------

ctrlLED:	;Controla acender/apagar do LED
		push 	cx
		mov 	cx,65000

loopDelayLED:
		push 	cx
	  mov 	cx,800

loopDelayLED2:
		loop 	loopDelayLED2

		pop 	cx
		loop 	loopDelayLED
		pop 	cx
		ret


;--------------------------------------------------------------------------------------------------------
;   Informacoes
;--------------------------------------------------------------------------------------------------------

informacoes:

  ; borda externa

		mov		ax, 5    ; x1
		push 	ax
		mov		ax, 5    ;  y1
		push 	ax

		mov		ax, 5    ; x2
		push	ax
		mov		ax, 475  ; y2
		push 	ax

		call 	line

		mov		ax, 630  ; x1
		push 	ax
		mov		ax, 5    ; y1
		push 	ax

		mov		ax, 630  ; x2
		push	ax
		mov		ax, 475  ; y2
		push 	ax

		call 	line

		mov		ax, 5    ; x1
		push 	ax
		mov		ax, 5    ; y1
		push 	ax

		mov		ax, 630  ; x2
		push	ax
		mov		ax, 5    ; y2
		push 	ax

  		  call line

		mov		ax, 5    ; x1
		push 	ax
		mov		ax, 475  ; y1
		push 	ax

		mov		ax, 630  ; x2
		push	ax
		mov		ax, 475  ; y2
		push 	ax

    call line

  ; escreve ' Para sair do programa pressionar Q '

		mov 	cx,36			;numero de caracteres
		mov 	bx,0
		mov 	dh,21			;linha 0-29
		mov 	dl,1			;coluna 0-79

escreveSair:
		call 	cursor
		mov 	al,[bx+SAIR]
		call 	caracter
		inc 	bx			;proximo caracter
		inc 	dl			;avanca a coluna
		loop 	escreveSair

  ; escreve cabecalho:

		mov 	cx,45			;numero de caracteres
		mov 	bx,0
		mov 	dh,24			;linha 0-29
		mov 	dl,1			;coluna 0-79

escreveCabecalho:
		call 	cursor
		mov 	al,[bx+CABECALHO]
		call 	caracter
		inc 	bx			;proximo caracter
		inc		dl			;avanca a coluna
		loop 	escreveCabecalho

  ; escreve nomes

		mov 	cx,7			;numero de caracteres
		mov 	bx,0
		mov 	dh,26			;linha 0-29
		mov 	dl,1			;coluna 0-79

escreverBBS:
		call 	cursor
		mov 	al,[bx+BBS]
		call 	caracter
		inc 	bx			;proximo caracter
		inc		dl			;avanca a coluna
		loop  escreverBBS

		mov 	cx,6			;numero de caracteres
		mov 	bx,0
		mov 	dh,27			;linha 0-29
		mov 	dl,1			;coluna 0-79

escreverDYE:
		call 	cursor
		mov 	al,[bx+DYE]
		call 	caracter
		inc 	bx			;proximo caracter
		inc		dl			;avanca a coluna
		loop 	escreverDYE

		mov 	cx,8			;numero de caracteres
		mov 	bx,0
		mov 	dh,28			;linha 0-29
		mov 	dl,1			;coluna 0-79

escreverWALLA:
		call 	cursor
		mov 	al,[bx+WALLA]
		call 	caracter
		inc 	bx			;proximo caracter
		inc		dl			;avanca a coluna
		loop 	escreverWALLA

		ret


;--------------------------------------------------------------------------------------------------------
;   Funções para desenhar as setas
;--------------------------------------------------------------------------------------------------------

SetaInterna1:
		pusha

		mov 	word[seta_x1],433
		mov 	word[seta_x2],453
		mov 	word[seta_y],120
		mov 	word[seta_y2],145
		call 	desenhaSetaCorpoPonta

		mov 	word[seta_x1],423
		mov 	word[seta_y],145
		mov 	word[seta_x2],443
		mov 	word[seta_y2],170
		call 	desenharSetaDiagonal

		mov 	word[seta_x1],443
		mov 	word[seta_y],170
		mov 	word[seta_x2],463
		mov 	word[seta_y2],145
		call 	desenharSetaDiagonal

		mov 	word[seta_x1],423
		mov 	word[seta_y],145
		mov 	word[seta_x2],433
		call 	desenharSetaReta

		mov 	word[seta_x1],453
		mov 	word[seta_y],145
		mov 	word[seta_x2],463
		call 	desenharSetaReta

		popa
		ret

SetaInterna2:
		pusha

		mov 	word[seta_x1],433
		mov 	word[seta_x2],453
		mov 	word[seta_y],225
		mov 	word[seta_y2],245
		call 	desenhaSetaCorpoMeio

		mov 	word[seta_x1],423
		mov 	word[seta_y],245
		mov 	word[seta_x2],443
		mov 	word[seta_y2],270
		call 	desenharSetaDiagonal

		mov 	word[seta_x1],443
		mov 	word[seta_y],270
		mov 	word[seta_x2],463
		mov 	word[seta_y2],245
		call 	desenharSetaDiagonal

		mov 	word[seta_x1],423
		mov 	word[seta_y],225
		mov 	word[seta_x2],443
		mov 	word[seta_y2],200
		call 	desenharSetaDiagonal

		mov 	word[seta_x1],443
		mov 	word[seta_y],200
		mov 	word[seta_x2],463
		mov 	word[seta_y2],225
		call 	desenharSetaDiagonal

		mov 	word[seta_x1],423
		mov 	word[seta_y],225
		mov 	word[seta_x2],433
		call 	desenharSetaReta

		mov 	word[seta_x1],453
		mov 	word[seta_y],225
		mov 	word[seta_x2],463
		call 	desenharSetaReta

		mov 	word[seta_x1],423
		mov 	word[seta_y],245
		mov 	word[seta_x2],433
		call 	desenharSetaReta

		mov 	word[seta_x1],453
		mov 	word[seta_y],245
		mov 	word[seta_x2],463
		call 	desenharSetaReta

		popa
		ret

SetaInterna3:
		pusha

		mov 	word[seta_x1],433
		mov 	word[seta_x2],453
		mov 	word[seta_y],330
		mov 	word[seta_y2],350
		call 	desenhaSetaCorpoMeio

		mov 	word[seta_x1],423
		mov 	word[seta_y],350
		mov 	word[seta_x2],443
		mov 	word[seta_y2],375
		call 	desenharSetaDiagonal

		mov 	word[seta_x1],443
		mov 	word[seta_y],375
		mov 	word[seta_x2],463
		mov 	word[seta_y2],350
		call 	desenharSetaDiagonal

		mov 	word[seta_x1],423
		mov 	word[seta_y],330
		mov 	word[seta_x2],443
		mov 	word[seta_y2],305
		call 	desenharSetaDiagonal

		mov 	word[seta_x1],443
		mov 	word[seta_y],305
		mov 	word[seta_x2],463
		mov 	word[seta_y2],330
		call 	desenharSetaDiagonal

		mov 	word[seta_x1],423
		mov 	word[seta_y],350
		mov 	word[seta_x2],433
		call 	desenharSetaReta

		mov 	word[seta_x1],453
		mov 	word[seta_y],350
		mov 	word[seta_x2],463
		call 	desenharSetaReta

		mov 	word[seta_x1],423
		mov 	word[seta_y],330
		mov 	word[seta_x2],433
		call 	desenharSetaReta

		mov 	word[seta_x1],453
		mov 	word[seta_y],330
		mov 	word[seta_x2],463
		call 	desenharSetaReta


		popa
		ret

SetaInterna4:
		pusha

		mov 	word[seta_x1],433
		mov 	word[seta_x2],453
		mov 	word[seta_y],450
		mov 	word[seta_y2],430
		call 	desenhaSetaCorpoPonta

		mov 	word[seta_x1],423
		mov 	word[seta_y],430
		mov 	word[seta_x2],443
		mov 	word[seta_y2],405
		call 	desenharSetaDiagonal

		mov 	word[seta_x1],443
		mov 	word[seta_y],405
		mov 	word[seta_x2],463
		mov 	word[seta_y2],430
		call 	desenharSetaDiagonal

		mov 	word[seta_x1],423
		mov 	word[seta_y],430
		mov 	word[seta_x2],433
		call 	desenharSetaReta

		mov 	word[seta_x1],453
		mov 	word[seta_y],430
		mov 	word[seta_x2],463
		call 	desenharSetaReta

		popa
		ret



SetaExterna1:
		pusha

		mov 	word[seta_x1],568
		mov 	word[seta_x2],588
		mov 	word[seta_y],120
		mov 	word[seta_y2],145
		call 	desenhaSetaCorpoPonta

		mov 	word[seta_x1],558
		mov 	word[seta_y],145
		mov 	word[seta_x2],578
		mov 	word[seta_y2],170
		call 	desenharSetaDiagonal

		mov 	word[seta_x1],578
		mov 	word[seta_y],170
		mov 	word[seta_x2],598
		mov 	word[seta_y2],145
		call 	desenharSetaDiagonal

		mov 	word[seta_x1],558
		mov 	word[seta_y],145
		mov 	word[seta_x2],568
		call 	desenharSetaReta

		mov 	word[seta_x1],588
		mov 	word[seta_y],145
		mov 	word[seta_x2],598
		call 	desenharSetaReta

		popa
		ret


SetaExterna2subindo:
		pusha

		mov 	word[seta_x1],568
		mov 	word[seta_x2],588
		mov 	word[seta_y],238
		mov 	word[seta_y2],245
		call 	desenhaSetaCorpoPonta

		mov 	word[seta_x1],558
		mov 	word[seta_y],245
		mov 	word[seta_x2],578
		mov 	word[seta_y2],270
		call 	desenharSetaDiagonal

		mov 	word[seta_x1],578
		mov 	word[seta_y],270
		mov 	word[seta_x2],598
		mov 	word[seta_y2],245
		call 	desenharSetaDiagonal

		mov 	word[seta_x1],558
		mov 	word[seta_y],245
		mov 	word[seta_x2],568
		call 	desenharSetaReta

		mov 	word[seta_x1],588
		mov 	word[seta_y],245
		mov 	word[seta_x2],598
		call 	desenharSetaReta


    popa
    ret

SetaExterna2descendo:
    pusha


		mov 	word[seta_x1],568
		mov 	word[seta_x2],588
		mov 	word[seta_y],232
		mov 	word[seta_y2],225
		call 	desenhaSetaCorpoPonta

		mov 	word[seta_x1],558
		mov 	word[seta_y],225
		mov 	word[seta_x2],578
		mov 	word[seta_y2],200
		call 	desenharSetaDiagonal

		mov 	word[seta_x1],578
		mov 	word[seta_y],200
		mov 	word[seta_x2],598
		mov 	word[seta_y2],225
		call 	desenharSetaDiagonal


		mov 	word[seta_x1],558
		mov 	word[seta_y],225
		mov 	word[seta_x2],568
		call 	desenharSetaReta

		mov 	word[seta_x1],588
		mov 	word[seta_y],225
		mov 	word[seta_x2],598
		call 	desenharSetaReta

		popa
		ret

SetaExterna3subindo:
		pusha

		mov 	word[seta_x1],568
		mov 	word[seta_x2],588
		mov 	word[seta_y],343
		mov 	word[seta_y2],350
		call 	desenhaSetaCorpoPonta

		mov 	word[seta_x1],558
		mov 	word[seta_y],350
		mov 	word[seta_x2],578
		mov 	word[seta_y2],375
		call 	desenharSetaDiagonal

		mov 	word[seta_x1],578
		mov 	word[seta_y],375
		mov 	word[seta_x2],598
		mov 	word[seta_y2],350
		call 	desenharSetaDiagonal

		mov 	word[seta_x1],558
		mov 	word[seta_y],350
		mov 	word[seta_x2],568
		call 	desenharSetaReta

		mov 	word[seta_x1],588
		mov 	word[seta_y],350
		mov 	word[seta_x2],598
		call 	desenharSetaReta



    popa
    ret

SetaExterna3descendo:
    pusha

		mov 	word[seta_x1],568
		mov 	word[seta_x2],588
		mov 	word[seta_y],337
		mov 	word[seta_y2],330
		call 	desenhaSetaCorpoPonta

		mov 	word[seta_x1],558
		mov 	word[seta_y],330
		mov 	word[seta_x2],578
		mov 	word[seta_y2],305
		call 	desenharSetaDiagonal

		mov 	word[seta_x1],578
		mov 	word[seta_y],305
		mov 	word[seta_x2],598
		mov 	word[seta_y2],330
		call 	desenharSetaDiagonal

		mov 	word[seta_x1],558
		mov 	word[seta_y],330
		mov 	word[seta_x2],568
		call 	desenharSetaReta

		mov 	word[seta_x1],588
		mov 	word[seta_y],330
		mov 	word[seta_x2],598
		call 	desenharSetaReta


		popa
		ret

SetaExterna4:
		pusha

		mov 	word[seta_x1],568
		mov 	word[seta_x2],588
		mov 	word[seta_y],450
		mov 	word[seta_y2],430
		call 	desenhaSetaCorpoPonta

		mov 	word[seta_x1],558
		mov 	word[seta_y],430
		mov 	word[seta_x2],578
		mov 	word[seta_y2],405
		call 	desenharSetaDiagonal

		mov 	word[seta_x1],578
		mov 	word[seta_y],405
		mov 	word[seta_x2],598
		mov 	word[seta_y2],430
		call 	desenharSetaDiagonal

		mov 	word[seta_x1],558
		mov 	word[seta_y],430
		mov 	word[seta_x2],568
		call 	desenharSetaReta

		mov 	word[seta_x1],588
		mov 	word[seta_y],430
		mov 	word[seta_x2],598
		call 	desenharSetaReta
		popa
		ret



desenharSetaDiagonal:


		mov 	ax,word[seta_x1]
		push 	ax
		mov 	ax,word[seta_y]
		push 	ax
		mov 	ax,word[seta_x2]
		push 	ax
		mov 	ax,word[seta_y2]
		push 	ax

		call 	line

		ret

desenharSetaReta:


		mov 	ax,word[seta_x1]
		push 	ax
		mov 	ax,word[seta_y]
		push 	ax
		mov 	ax,word[seta_x2]
		push 	ax
		mov 	ax,word[seta_y]
		push 	ax

		call 	line

		ret



desenhaSetaCorpoPonta:

		mov 	ax,word[seta_x1]
		push 	ax
		mov 	ax,word[seta_y]
		push 	ax
		mov 	ax,word[seta_x1]
		push 	ax
		mov 	ax,word[seta_y2]
		push 	ax

		call 	line

		mov 	ax,word[seta_x2]
		push 	ax
		mov 	ax,word[seta_y]
		push 	ax
		mov 	ax,word[seta_x2]
		push 	ax
		mov 	ax,word[seta_y2]
		push 	ax

		call 	line


		mov 	ax,word[seta_x1]
		push 	ax
		mov 	ax,word[seta_y]
		push 	ax
		mov 	ax,word[seta_x2]
		push 	ax
		mov 	ax,word[seta_y]
		push 	ax

		call 	line


		ret



desenhaSetaCorpoMeio:

		mov 	ax,word[seta_x1]
		push 	ax
		mov 	ax,word[seta_y]
		push 	ax
		mov 	ax,word[seta_x1]
		push 	ax
		mov 	ax,word[seta_y2]
		push 	ax

		call 	line

		mov 	ax,word[seta_x2]
		push 	ax
		mov 	ax,word[seta_y]
		push 	ax
		mov 	ax,word[seta_x2]
		push 	ax
		mov 	ax,word[seta_y2]
		push 	ax

		call 	line

		ret



atualizarAndar:
		pusha

		mov   dh,4		;linha 0-29
    	mov   dl,16		;coluna 0-79
		call  cursor

		mov 	bl,[andar_atual]
		mov   al,'4'
		cmp 	bl,andar_4
		je 		escreveAndar
		dec   al
		cmp 	bl,andar_3
		je 		escreveAndar
		dec   al
		cmp 	bl,andar_2
		je 		escreveAndar
		dec 	al
		cmp 	bl,andar_1
		jne 	fimEscreverAndar

escreveAndar:
		mov 	byte[cor],branco_intenso
    call  caracter

fimEscreverAndar:
    popa
    ret

escreverParado:

		mov 	word[cor], branco_intenso
		mov   cx,8
		mov   bx,0
		mov   dh,5;linha 0-29
		mov   dl,23;coluna 0-79

loopEscreverParado:
		call 	cursor
		mov 	al,[bx+PARADO]
		call 	caracter
		inc 	bx			;proximo caracter
		inc 	dl			;avanca a coluna
		loop  loopEscreverParado

		ret

escreverSubindo:

		mov 	word[cor], branco_intenso
		mov   cx,8
		mov   bx,0
		mov   dh,5;linha 0-29
		mov   dl,23;coluna 0-79

loopEscreverSubindo:
		call 	cursor
		mov 	al,[bx+SUBINDO]
		call 	caracter
		inc 	bx			;proximo caracter
		inc 	dl			;avanca a coluna
		loop	loopEscreverSubindo

		ret

escreverDescendo:

		mov 	word[cor], branco_intenso
		mov   cx,8
		mov   bx,0
		mov   dh,5;linha 0-29
		mov   dl,23;coluna 0-79

loopEscreverDescendo:
		call 	cursor
		mov 	al,[bx+DESCENDO]
		call 	caracter
		inc 	bx			;proximo caracter
		inc 	dl			;avanca a coluna
		loop  loopEscreverDescendo

		ret

escreverFuncionando:

		mov 	word[cor], branco_intenso
		mov   cx,11
		mov   bx,0
		mov   dh,6		;linha 0-29
		mov   dl,21		;coluna 0-79

loopEscreverFuncionando:
		call 	cursor
		mov 	al,[bx+FUNCIONANDO]
		call 	caracter
		inc 	bx			;proximo caracter
		inc 	dl			;avanca a coluna
		loop  loopEscreverFuncionando

		ret

escreverEmergencia:

		mov 	word[cor], vermelho
		mov   cx,11
		mov   bx,0
		mov   dh,6				;linha 0-29
		mov   dl,21				;coluna 0-79

loopEscreveEmergencia:
		call 	cursor
		mov 	al,[bx+EMERGENCIA]
		call 	caracter
		inc 	bx				;proximo caracter
		inc 	dl				;avanca a coluna
		loop  loopEscreveEmergencia

		ret


;--------------------------------------------------------------------------------------------------------
;   Tela inicial de calibração
;--------------------------------------------------------------------------------------------------------

telaInicial:

	; escreve 'Calibrando o Elevador...'

		mov 	cx,24			;numero de caracteres
		mov 	bx,0
		mov 	dh,8			;linha 0-29
		mov 	dl,27			;coluna 0-79

escreverCalibracao:
		call 	cursor
		mov 	al,[bx+CALIBRACAO]
		call 	caracter
		inc 	bx			  ;proximo caracter
		inc		dl			  ;avanca a coluna
		loop  escreverCalibracao

  ; escreve ' Aperte ESPACO no quarto andar '

		mov 	cx,31			;numero de caracteres
		mov 	bx,0
		mov 	dh,10			;linha 0-29
		mov 	dl,22			;coluna 0-79

escreverAperteEspaco:
		call 	cursor
		mov 	al,[bx+APERTE_ESPACO]
		call 	caracter
		inc 	bx			;proximo caracter
		inc		dl			;avanca a coluna
		loop  escreverAperteEspaco
		ret


;--------------------------------------------------------------------------------------------------------
;   Tela Principal
;--------------------------------------------------------------------------------------------------------

telaPrincipal:

  ; escreve ' Andar Atual: '

    mov   cx,14			;numero de caracteres
    mov   bx,0
    mov   dh,4			;linha 0-29
    mov   dl,2			;coluna 0-79

escreverAndarAtual:
    call 	cursor
    mov 	al,[bx+ANDAR]
    call 	caracter
    inc 	bx				;proximo caracter
    inc 	dl				;avanca a coluna
    loop  escreverAndarAtual

  ; escreve ' Estado do Elevador: '

    mov   cx,21			;numero de caracteres
    mov   bx,0
    mov   dh,5			;linha 0-29
    mov   dl,2			;coluna 0-79

escreverEstadoElevador:
    call 	cursor
    mov 	al,[bx+ESTADO]
    call 	caracter
    inc 	bx				;proximo caracter
    inc 	dl				;avanca a coluna
    loop  escreverEstadoElevador

  ; escreve ' Modo de Operacao: '

    mov   cx,19			;numero de caracteres
    mov   bx,0
    mov   dh,6			;linha 0-29
    mov   dl,2			;coluna 0-79

escreverModoOperacao:
    call 	cursor
    mov 	al,[bx+MODO_OPERACAO]
    call 	caracter
    inc 	bx				;proximo caracter
    inc 	dl				;avanca a coluna
    loop  escreverModoOperacao


  ; primeira linha

    mov		ax, 380 ; x1
    push 	ax
    mov		ax, 96 	; y1
    push 	ax

    mov		ax, 630 ; x2
    push 	ax
    mov		ax, 96 	; y2
    push 	ax

    call 	line

  ; segunda linha

    mov		ax, 380 ; x1
    push 	ax
    mov		ax, 192 ; y1
    push 	ax

    mov		ax, 630 ; x2
    push 	ax
    mov		ax, 192 ; y2
    push 	ax

    call 	line

  ; terceira linha

    mov		ax, 380 ; x1
    push 	ax
    mov		ax, 288 ; y1
    push 	ax

    mov		ax, 630 ; x2
    push 	ax
    mov		ax, 288 ; y2
    push 	ax

    call 	line

  ; quarta linha

    mov		ax, 380 ; x1
    push 	ax
    mov		ax, 384 ; y1
    push 	ax

    mov		ax, 630 ; x2
    push 	ax
    mov		ax, 384 ; y2
    push 	ax

    call 	line

  ; primeira coluna

    mov		ax, 380 ; x1
    push 	ax
    mov		ax, 5 	; y1
    push 	ax

    mov		ax, 380 ; x2
    push	ax
    mov		ax, 475 ; y2
    push 	ax

    call 	line

  ; segunda coluna

    mov		ax, 510 ; x1
    push 	ax
    mov		ax, 5 	; y1
    push 	ax

    mov		ax, 510 ; x2
    push 	ax
    mov		ax, 475 ; y2
    push 	ax

    call 	line


  ; escreve 'Chamadas'

    mov   cx,8			;numero de caracteres
    mov   bx,0
    mov   dh,26			;linha 0-29
    mov   dl,51			;coluna 0-79

escreverChamada1:
    call 	cursor
    mov 	al,[bx+CHAMADA]
    call 	caracter
    inc 	bx				;proximo caracter
    inc 	dl				;avanca a coluna
    loop  escreverChamada1

    mov   cx,8			;numero de caracteres
    mov   bx,0
    mov   dh,26			;linha 0-29
    mov   dl,68			;coluna 0-79

escreverChamada2:
    call 	cursor
    mov 	al,[bx+CHAMADA]
    call 	caracter
    inc 	bx				;proximo caracter
    inc 	dl				;avanca a coluna
    loop  escreverChamada2

  ; escreve 'INTERNAS'

    mov   cx,8			;numero de caracteres
    mov   bx,0
    mov   dh,27			;linha 0-29
    mov   dl,51			;coluna 0-79

escreverInternas:
    call 	cursor
    mov 	al,[bx+INTERNA]
    call 	caracter
    inc 	bx				;proximo caracter
    inc 	dl				;avanca a coluna
    loop  escreverInternas

  ; escreve 'EXTERNAS'

    mov   cx,8			;numero de caracteres
    mov   bx,0
    mov   dh,27			;linha 0-29
    mov   dl,68			;coluna 0-79

escreverExternas:
    call 	cursor
    mov 	al,[bx+EXTERNA]
    call 	caracter
    inc 	bx				;proximo caracter
    inc 	dl				;avanca a coluna
    loop  escreverExternas

    call  SetaInterna1
    call  SetaInterna2
    call  SetaInterna3
    call  SetaInterna4
    call 	SetaExterna1
    call  SetaExterna3subindo
    call  SetaExterna3descendo
    call  SetaExterna2subindo
    call  SetaExterna2descendo
    call 	SetaExterna4

    ret



; --------------------------------------------------------------------------------------------------------
;   Funcao line
; --------------------------------------------------------------------------------------------------------
; push x1; push y1; push x2; push y2; call line;  (x<640, y<480)

line:
		push	bp
		mov		bp,sp
		pushf                        ;coloca os flags na pilha
		push 	ax
		push 	bx
		push	cx
		push	dx
		push	si
		push	di
		mov		ax,[bp+10]   ; resgata os valores das coordenadas
		mov		bx,[bp+8]    ; resgata os valores das coordenadas
		mov		cx,[bp+6]    ; resgata os valores das coordenadas
		mov		dx,[bp+4]    ; resgata os valores das coordenadas
		cmp		ax,cx
		je		line2
		jb		line1
		xchg	ax,cx
		xchg	bx,dx
		jmp		line1

line2:		; deltax=0
		cmp		bx,dx  ;subtrai dx de bx
		jb		line3
		xchg	bx,dx        ;troca os valores de bx e dx entre eles

line3:	; dx > bx
		push	ax
		push	bx
		call 	plot_xy
		cmp		bx,dx
		jne		line31
		jmp		fim_line

line31:
		inc		bx
		jmp		line3
;deltax <>0

line1:
; comparar modulos de deltax e deltay sabendo que cx>ax
; cx > ax
		push	cx
		sub		cx,ax
		mov		[deltax],cx
		pop		cx
		push	dx
		sub		dx,bx
		ja		line32
		neg		dx

line32:
		mov		[deltay],dx
		pop		dx

		push	ax
		mov		ax,[deltax]
		cmp		ax,[deltay]
		pop		ax
		jb		line5

	; cx > ax e deltax>deltay
		push	cx
		sub		cx,ax
		mov		[deltax],cx
		pop		cx
		push	dx
		sub		dx,bx
		mov		[deltay],dx
		pop		dx

		mov		si,ax

line4:
		push	ax
		push	dx
		push	si
		sub		si,ax	;(x-x1)
		mov		ax,[deltay]
		imul	si
		mov		si,[deltax]		;arredondar
		shr		si,1
; se numerador (DX)>0 soma se <0 subtrai
		cmp		dx,0
		jl		ar1
		add		ax,si
		adc		dx,0
		jmp		arc1

ar1:
		sub		ax,si
		sbb		dx,0

arc1:
		idiv	word [deltax]
		add		ax,bx
		pop		si
		push	si
		push	ax
		call	plot_xy
		pop		dx
		pop		ax
		cmp		si,cx
		je		fim_line
		inc		si
		jmp		line4

line5:
		cmp		bx,dx
		jb 		line7
		xchg	ax,cx
		xchg	bx,dx

line7:
		push	cx
		sub		cx,ax
		mov		[deltax],cx
		pop		cx
		push	dx
		sub		dx,bx
		mov		[deltay],dx
		pop		dx

		mov		si,bx

line6:
		push	dx
		push	si
		push	ax
		sub		si,bx	;(y-y1)
		mov		ax,[deltax]
		imul	si
		mov		si,[deltay]		;arredondar
		shr		si,1
; se numerador (DX)>0 soma se <0 subtrai
		cmp		dx,0
		jl		ar2
		add		ax,si
		adc		dx,0
		jmp		arc2

ar2:
		sub		ax,si
		sbb		dx,0

arc2:
		idiv	word [deltay]
		mov		di,ax
		pop		ax
		add		di,ax
		pop		si
		push	di
		push	si
		call	plot_xy
		pop		dx
		cmp		si,dx
		je		fim_line
		inc		si
		jmp		line6

fim_line:
		pop		di
		pop		si
		pop		dx
		pop		cx
		pop		bx
		pop		ax
		popf
		pop		bp
		ret		8

;--------------------------------------------------------------------------------------------------------
;   Funcao plot_xy
;--------------------------------------------------------------------------------------------------------

; push x; push y; call plot_xy;  (x<640, y<480)
; cor definida na variavel cor
plot_xy:
		push	bp
		mov		bp,sp
		pushf
		pusha
    mov     ah,0ch
    mov     al,[cor]
    mov     bh,0
    mov     dx,480
		sub		dx,[bp+4]
    mov     cx,[bp+6]
    int    	10h
		popa
		popf
		pop		bp
		ret		4

;--------------------------------------------------------------------------------------------------------
;   Função aloca o cursor
;--------------------------------------------------------------------------------------------------------
; dh = linha (0-29) e  dl=coluna  (0-79)

cursor:
		pushf
		push     ax
		push     bx
		push     cx
		push     dx
		push     si
		push     di
		push     bp
		mov      ah,2
		mov      bh,0
		int      10h
		pop	     bp
		pop	     di
		pop	     si
		pop	     dx
		pop	     cx
		pop	     bx
		pop	     ax
		popf
		ret

;--------------------------------------------------------------------------------------------------------
;  Funçao caracter escrito na posição do cursor - Inicio
;--------------------------------------------------------------------------------------------------------
; al= caracter a ser escrito
; cor definida na variavel cor

caracter:
		pushf
		push     ax
		push     bx
		push     cx
		push     dx
		push     si
		push     di
		push     bp
  	mov      ah,9
  	mov      bh,0
  	mov      cx,1
 		mov      bl,[cor]
  	int      10h
		pop	     bp
		pop	     di
		pop	     si
		pop	     dx
		pop	     cx
		pop	     bx
		pop	     ax
		popf
		ret


; --------------------------------------------------------------------------------------------------------
; INTERRUPCAO: LEITURA  DO TECLADO
; --------------------------------------------------------------------------------------------------------

keyint:
		pusha

;Trata as interrupções recebidas do Teclado
    IN    AL, kb_data
    mov   [tecla],al
    IN    AL, kb_ctl
    OR    AL, 80h
    OUT   kb_ctl, AL
    AND   AL, 7Fh
    OUT   kb_ctl, AL
    MOV   AL, eoi
    OUT   pictrl, AL

;Atualiza as chamadas internas
;Tecla de sair do programa

		cmp   byte[tecla],10h				;verifica se o q foi apertado, caso sim sai do programa
		jne		verificaESC
		call 	sair

;Emergencia

verificaESC:
		cmp		byte[tecla],tecla_ESC
		jne		verificaTecla1
		cmp		byte[sinal_emergencia],0
		jne		verificaESCsaida
		call 	escreverEmergencia
		mov		byte[sinal_emergencia],1
		jmp		verificaTecla1

verificaESCsaida:
		call	escreverFuncionando
		mov		byte[sinal_emergencia],0


verificaTecla1:
		cmp   byte[tecla],tecla_1									;verifica se o 1 foi pressionado
		jne   verificaTecla2
		or    byte[chamadas_internas],andar_1			;confirma que o 1 foi pressionado
		call  atualizaSetaInterna1

verificaTecla2:
		cmp   byte[tecla],tecla_2									;verifica se o 2 foi pressionado
		jne   verificaTecla3
		or    byte[chamadas_internas],andar_2			;confirma que o 2 foi pressionado
		call 	atualizaSetaInterna2

verificaTecla3:
		cmp   byte[tecla],tecla_3									;verifica se o 3 foi pressionado
		jne   verificaTecla4
		or    byte[chamadas_internas],andar_3			;confirma que o 3 foi pressionado
		call 	atualizaSetaInterna3

verificaTecla4:
		cmp   byte[tecla],tecla_4									;verifica se o 4 foi pressionado
		jne   fim_key_int
		or    byte[chamadas_internas],andar_4			;confirma que o 4 foi pressionado
		call 	atualizaSetaInterna4

fim_key_int:
		popa
    IRET



;--------------------------------------------------------------------------------------------------------
;   DADOS - variaveis
;--------------------------------------------------------------------------------------------------------

segment data
  ; variaveis para leitura do teclado
    kb_data 				EQU 	60h  					;PORTA DE LEITURA DE TECLADO
    kb_ctl  				EQU 	61h  					;PORTA DE RESET PARA PEDIR NOVA INTERRUPCAO
    pictrl  				EQU 	20h
    eoi     				EQU 	20h
    int9    				EQU 	9h
    cs_dos  				DW  	1
    offset_dos  		DW 		1

    tecla   				resb  8

    ;I R G B COR
    ;0 0 0 0 preto
    ;0 0 0 1 azul
    ;1 1 1 1 branco intenso

		cor									db		branco_intenso
    preto 							equ		0
    azul            		equ		1
    vermelho      			equ		4
    branco_intenso			equ		15

		deltax					dw		0
		deltay					dw		0
		seta_x1					dw   	0
		seta_x2					dw		0
		seta_y					dw   	0
		seta_y2					dw   	0

		elevador_descendo  	equ   	10000000b
		elevador_subindo  	equ   	01000000b
		elevador_parado			equ   	00000000b

    tecla_ESC      			equ    	01h
    tecla_1       			equ    	02h
    tecla_2      				equ    	03h
    tecla_3      				equ    	04h
    tecla_4     				equ    	05h
    tecla_Q	     				equ    	10h
    tecla_espaco     		equ    	39h

		estado_parado			equ    	02h
		estado_subindo			equ    	01h
		estado_descendo		equ    	00h
		nao_emergencia			equ    	00h

		andar_1_subindo			equ    	00000001b
		andar_2_descendo		equ    	00000010b
		andar_2_subindo			equ    	00000100b
		andar_3_descendo		equ    	00001000b
		andar_3_subindo			equ    	00010000b
		andar_4_descendo		equ    	00100000b
		acha_ranhura				equ    	01000000b

    andar_1      			equ    	1	;0
    andar_2      			equ    	2	;1
    andar_3      			equ    	4	;2
    andar_4      			equ    	8	;3

		estado_anterior    					db     	0
		estado_elevador   					db     	0

		sinal_emergencia    				db     	0

		ranhura_lida    					db     	0
		ranhura_anterior   				db     	0

		sentido_elevador     				db     	0
		sentido_elevador_anterior 	db     	0

		led_externo   							db     	0

    andar_atual       					db     	0
    chamadas_internas    		    db     	0
    externas_subida   					db     	0
    externas_descida    				db 	   	0
    externas_subida_anterior   	db     	0	;Bkp para ser usado na atualiazação da parte grafica
    externas_descida_anterior   db 	   	0	;Bkp para ser usado na atualiazação da parte grafica
    modo_anterior								db	   	0

; valores a serem ecritos no programa
		DYE         	  	db 	' Dyego'												
		BBS       			db 	' Brenno'
	    WALLA               db  ' Wallace'

		CABECALHO  			db 	' Projeto Final de Sistemas Embarcados 2019-1 '   ;45 caracteres
		SAIR						db	' Para sair do programa pressionar Q '						;36 caracteres
		APERTE_ESPACO  	db 	' Aperte ESPACO no quarto andar '									;31 caracteres
		ANDAR    				db 	' Andar Atual: '																	;12 caracteres
		ESTADO					db 	' Estado do Elevador: '														;21 caracteres
		CALIBRACAO      db 	'Calibrando o Elevador...'				    			  		;24 caracteres
		MODO_OPERACAO 	db 	' Modo de Operacao: '															;19 caracteres
		CHAMADA					db 	'Chamadas'																				;8 caracteres
		INTERNA					db 	'INTERNAS'																				;8 caracteres
		EXTERNA					db 	'EXTERNAS'																				;8 caracteres
		EMERGENCIA     	db 	'EMERGENCIA '																			;11 caracteres
		FUNCIONANDO    	db 	'Funcionando'																			;11 caracteres
		PARADO					db 	'Parado  '																				;8 caracteres
		SUBINDO 				db 	'Subindo '													         			;8 caracteres
		DESCENDO 				db 	'Descendo'												         				;8 caracteres
segment stack stack
    resb 256
stacktop:
