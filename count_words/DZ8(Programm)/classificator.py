#!/usr/bin/env python3
import math

from pymystem3 import *	

p_c = 0.5

fin1  = open('class1', 'r')
fin2  = open('class2', 'r')

def delall(lst):
	l = []
	for n in lst:
	    if len(n) > 3:
	       l.append(n)
	return(l)

def get_norm_text (text):
	m    = Mystem()
	analized_text = m.lemmatize (text)
	return (analized_text)

class1 = delall(get_norm_text(fin1.read()))

class2 = delall(get_norm_text(fin2.read()))

vok = list(set(class1)) + list(set(class2))

def p_xj_cj(word, class_, test):
	p = (class_.count(word) + 1)/(len(test) + len(vok))
	return p

def classificate(file):
	test = delall(get_norm_text(open(file, 'r').read()))
	p_c1 = math.log(p_c)
	for word in test:
		p_c1 = p_c1 + math.log(p_xj_cj(word, class1, test))
	print('Вес первого класса : ', p_c1, '\n')
	p_c2 = math.log(p_c)
	for word in test:
		p_c2 = p_c2 + math.log(p_xj_cj(word, class2, test))
	print('Вес второго класса : ', p_c2, '\n')
	if p_c1 > p_c2:
		print('c1')
	else:
		print('c2')

classificate('article')
	