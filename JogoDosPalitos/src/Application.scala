/**
 * Created by eduardo on 28/03/15.
 */

import scala.collection.immutable.HashMap
import scala.util.Random

object Application {
	def main(args: Array[String]) {
		val numeroDeJogadores = lerNumeroDeJogadores
		começarRodada(criarJogadores(numeroDeJogadores), decidirQuemJogaPrimeiro(numeroDeJogadores))
	}

	// A maquina é sempre o jogador 0
	def começarRodada(jogadores : List[Jogador], primeiroJogador : Int): Unit = {
		jogadores.size match
		{
			case 1 => println("Acabou o jogo !")
			case _ =>
				val numeroDeJogadores = jogadores.size
				println("O número de palitos dos jogadores é: " + palitosDosJogadores(jogadores).mkString(", ") + "\n")
				println("Começando a rodada!")
				println("O primeiro a apostar será o jogador " + primeiroJogador)
				val jogadas = iniciarMap
				primeiroJogador match
				{
					case 1 =>
						jogarPrimeiro
					case _ =>
						jogar
				}

				def jogarPrimeiro: Unit ={
					pedirParaJogar
					dizerAposta(apostaRandomica( jogadores ))
					esperarFimDaRodada
					dizerJogada(jogadaRandomica( jogadores(0).palitos ))
					val ganhadorDaRodada = perguntarQuemAcertou(numeroDeJogadores)
					ganhadorDaRodada match
					{
						case 0 =>
							começarRodada(jogadores, if (primeiroJogador < numeroDeJogadores) primeiroJogador+1 else 1 )
						case _ =>
							val jogadoresComPalitosAtualizados = retirarPalito(jogadores, ganhadorDaRodada)
							val a = verificarSeAlguemFoiEliminado(jogadoresComPalitosAtualizados)
							a match
							{
								case -1 =>
									começarRodada(jogadoresComPalitosAtualizados, if (primeiroJogador < numeroDeJogadores) primeiroJogador+1 else 1 )
								case x =>
									começarRodada(jogadoresComPalitosAtualizados.take(x) ++ jogadoresComPalitosAtualizados.drop(x+1), if (primeiroJogador < numeroDeJogadores) primeiroJogador+1 else 1 )
							}
					}
				}

				def jogar: Unit = {
					val apostas = lerApostasDeCadaJogador(jogadores, primeiroJogador)
					apostas.size match
					{
						case tamanho if tamanho > 0 =>
							val minhaJogada = jogadaRandomica( jogadores(0).palitos )
							val minhaAposta = jogadas.get( (jogadores(0).palitos :: jogadores(1).palitos :: apostas) :+ minhaJogada ).get
							dizerAposta(minhaAposta)
							esperarFimDaRodada
							dizerJogada(minhaJogada)
							val ganhadorDaRodada = perguntarQuemAcertou(numeroDeJogadores)
							ganhadorDaRodada match
							{
								case 0 =>
									começarRodada(jogadores, if (primeiroJogador < numeroDeJogadores) primeiroJogador+1 else 1 )
								case _ =>
									val jogadoresComPalitosAtualizados = retirarPalito(jogadores, ganhadorDaRodada)
									val a = verificarSeAlguemFoiEliminado(jogadoresComPalitosAtualizados)
									a match
									{
										case -1 =>
											começarRodada(          jogadoresComPalitosAtualizados, if (primeiroJogador < numeroDeJogadores) primeiroJogador+1 else 1 )
										case x =>
											começarRodada(jogadoresComPalitosAtualizados.take(x) ++ jogadoresComPalitosAtualizados.drop(x+1), if (primeiroJogador < numeroDeJogadores) primeiroJogador+1 else 1 )
									}
							}
						case _ =>
							println( "Algo deu muito errado!")
					}
				}
		}
	}

	def pedirParaJogar: Unit ={
		try {
			var resposta = "nao"
			while( resposta != "sim"){
				resposta = readLine("Todos já colocaram os palitos? Posso começar a rodada? \n")
			}
		}
		catch {
			case npe: NullPointerException =>
				println("Diga <sim>")
				lerNumeroDeJogadores
		}
	}

	def verificarSeAlguemFoiEliminado(jogadores : List[Jogador], index : Int = 0): Int ={
		jogadores.head.palitos match
		{
			case 0 => index
			case _ =>
				jogadores.tail.size match
				{
					case x if x > 0 => verificarSeAlguemFoiEliminado(jogadores.tail, index+1)
					case _ => -1
				}
		}
	}

	def palitosDosJogadores( jogadores : List[Jogador]): List[Int] ={
		var palitos = List[Int]()
		for( jogador <- jogadores){
			palitos = palitos :+ jogador.palitos
		}
		palitos
	}

	def retirarPalito(jogadores : List[Jogador], ganhadorDaRodada : Int, novosJogadores : List[Jogador] = List()) : List[Jogador] ={
		novosJogadores.size match
		{
			case x if x == ganhadorDaRodada-1 => retirarPalito(jogadores, ganhadorDaRodada, novosJogadores :+ new Jogador(jogadores(x).palitos-1, jogadores(x).ID))
			case x if x < jogadores.size => retirarPalito(jogadores,ganhadorDaRodada,novosJogadores :+ jogadores(x))
			case _ => novosJogadores
		}
	}

	def perguntarQuemAcertou(numeroDeJogadores : Int) : Int ={
		try {
			val input = readLine("Se alguem acertou o número de palitos informe o número desse jogador, se ninguem acertou digite 0 \n")
			input.toInt match {
				case x if x >= 0 | x <= numeroDeJogadores =>
					input.toInt
				case _ =>
					println("Você deve inserir um numero positivo positivo")
					perguntarQuemAcertou(numeroDeJogadores)
			}
		}
		catch {
			case nfe: NumberFormatException =>
				println("Você deve inserir um número")
				perguntarQuemAcertou(numeroDeJogadores)
			case npe: NullPointerException =>
				println("Você deve inserir um número")
				perguntarQuemAcertou(numeroDeJogadores)
		}
	}

	def dizerJogada( jogada : Int ): Unit ={
		println("Minha jogada é: " + jogada)
	}

	def dizerAposta( aposta : Int): Unit ={
		println("Minha aposta é: "+aposta)
	}

	def esperarFimDaRodada: Unit ={
		try {
			var resposta = "nao"
			while( resposta != "sim"){
				resposta = readLine("Posso mostrar meus palitos? \n")
			}
		}
		catch {
			case npe: NullPointerException =>
				println("Diga <sim>")
				lerNumeroDeJogadores
		}
	}

	def apostaRandomica(jogadores : List[Jogador]) : Int =  {
		val rand = new Random
		var totalDePalitos = 0
		for ( jogador <- jogadores){
			totalDePalitos = totalDePalitos + jogador.palitos
		}
		rand.nextInt( totalDePalitos+1 )
	}

	def jogadaRandomica( palitos : Int ): Int ={
		val rand = new Random
		rand.nextInt(palitos+1)
	}

	// Cria uma lista com com uma entrada pra cada jogador com valor 3, que representa o número inicial de palitos
	def criarJogadores(numeroDeJogadores: Int, index : Int = 0, listaDeJogadores: List[Jogador] = List()): List[Jogador] = {
		index match
		{
			case x if x >= numeroDeJogadores => listaDeJogadores
			case _ => criarJogadores( numeroDeJogadores, index+1, listaDeJogadores :+ new Jogador(2,(index+1).toString))
		}
	}

	// Gera um número random entre 1 e numeroDeJogadores
	def decidirQuemJogaPrimeiro(numeroDeJogadores: Int): Int = {
		val rand = new Random
		rand.nextInt(numeroDeJogadores) + 1
	}


	// Lê as apostas dos jogadores que jogam antes do computador do console
	def lerApostasDeCadaJogador(jogadores : List[Jogador], primeiroJogador: Int): List[Int] = {

		// Encontra os jogadores que jogam antes da maquina e retorna uma lista com seus IDS
		def quemJogaAntesDaMaquina (lista: List[String] = List(), index : Int = 0): List[String] = {
			index+1 match {
				case x if x < primeiroJogador =>
					quemJogaAntesDaMaquina(lista, index+1)
				case x if x >= primeiroJogador & x <= jogadores.size =>
					quemJogaAntesDaMaquina(lista :+ jogadores(index).ID, index+1)
				case _ =>
					lista
			}
		}

		val apostadores = quemJogaAntesDaMaquina()
		apostadores.size match {
			case 0 =>
				List()
			case _ =>
				val input = readLine("O(s) jogadore(s) - " + apostadores.mkString(", ")  + " - joga(m) antes do computador. Informe sua(s) aposta(s) no formato <a1,a2,..,ai>, começando com o jogador de número mais baixo: \n")
				input.split(",").length match {
					case x if x == apostadores.size =>
						val listaDeApostas = input.split(",").toList.map(x => x.toInt)
						listaDeApostas.size match {
							case 1 =>
								listaDeApostas
							case _ =>
								listaDeApostas.distinct.size match {
									case x if x == listaDeApostas.size =>
										listaDeApostas
									case _ =>
										println("Não são aceitas apostas repetidas, vamos denovo")
										lerApostasDeCadaJogador(jogadores, primeiroJogador)
								}
						}
					case _ =>
						println("Os dados inseridos estavam incorretos, vamos denovo")
						lerApostasDeCadaJogador(jogadores, primeiroJogador)
				}
		}
	}

	// Lê do console o número de jogadores que irão jogar
	def lerNumeroDeJogadores: Int = {
		try {
			val input = readLine("Informe o número de jogadores: \n")
			input.toInt match {
				case x if x > 0 =>
					input.toInt
				case _ =>
					println("Você deve inserir um numero positivo positivo")
					lerNumeroDeJogadores
			}
		}
		catch {
			case nfe: NumberFormatException =>
				println("Você deve inserir um número")
				lerNumeroDeJogadores
			case npe: NullPointerException =>
				println("Você deve inserir um número")
				lerNumeroDeJogadores
		}
	}

	def iniciarMap  = {
		HashMap(
			List(1, 1, 0, 0) -> 1,
			List(1, 1, 0, 1) -> 1,
			List(1, 1, 1, 0) -> 0,
			List(1, 1, 1, 1) -> 2,
			List(1, 1, 2, 0) -> 1,
			List(1, 1, 2, 1) -> 1,
			List(1, 2, 0, 0) -> 1,
			List(1, 2, 0, 1) -> 1,
			List(1, 2, 1, 0) -> 0,
			List(1, 2, 1, 1) -> 2,
			List(1, 2, 2, 0) -> 1,
			List(1, 2, 2, 1) -> 3,
			List(1, 2, 3, 0) -> 2,
			List(1, 2, 3, 1) -> 2,
			List(2, 1, 0, 0) -> 1,
			List(2, 1, 0, 1) -> 1,
			List(2, 1, 0, 2) -> 2,
			List(2, 1, 1, 0) -> 0,
			List(2, 1, 1, 1) -> 2,
			List(2, 1, 1, 2) -> 3,
			List(2, 1, 2, 0) -> 1,
			List(2, 1, 2, 1) -> 3,
			List(2, 1, 2, 2) -> 3,
			List(2, 1, 3, 0) -> 2,
			List(2, 1, 3, 1) -> 2,
			List(2, 1, 3, 2) -> 2,
			List(2, 2, 0, 0) -> 1,
			List(2, 2, 0, 1) -> 1,
			List(2, 2, 0, 2) -> 2,
			List(2, 2, 1, 0) -> 0,
			List(2, 2, 1, 1) -> 2,
			List(2, 2, 1, 2) -> 2,
			List(2, 2, 2, 0) -> 1,
			List(2, 2, 2, 1) -> 3,
			List(2, 2, 2, 2) -> 3,
			List(2, 2, 3, 0) -> 2,
			List(2, 2, 3, 1) -> 2,
			List(2, 2, 3, 2) -> 4,
			List(2, 2, 4, 0) -> 3,
			List(2, 2, 4, 1) -> 3,
			List(2, 2, 4, 2) -> 3)
	}
}
