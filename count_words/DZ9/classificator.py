#!/usr/bin/env python3
from bs4 import BeautifulSoup
from sklearn.naive_bayes import GaussianNB, BernoulliNB, MultinomialNB
from sklearn.feature_extraction.text import TfidfVectorizer, CountVectorizer
from sklearn import svm
from sklearn.metrics import f1_score
import numpy as np

def get_data(text):
    soup = BeautifulSoup(text, "lxml")
    sents  = soup.find_all('speech', {'type': 'direct'})
    labels = soup.find_all('evaluation')
    return zip(labels, sents)

def del_spaces(text):
	res = ''
	for char in text:
		if char != ' ':
			res += char
	return res

def del_wrong_labels(list_):
	res1 = []
	res2 = []

	for i in list_:
		string = del_spaces((str(i[0]).split('<evaluation>\n')[1]).split('\n      </evaluation>')[0])
		if string == '+' or \
		   string == '-' or \
		   string == '0':
			res1.append(string)
			res2.append((str(i[1]).split('<speech type="direct">')[1]).split('\n      </speech>')[0])
	return (res1, res2)



fin1 = open("train/news_eval_train.xml", "r")
fin2 = open("test/news_eval_test.xml", "r")
fout = open("res", "w")

train_sents = get_data(fin1.read())
train_sents = del_wrong_labels(train_sents)

test_sents = get_data(fin2.read())
test_sents = del_wrong_labels(test_sents)

tfidf_vectorizer = TfidfVectorizer(input = 'content', analyzer = 'char')

tf_vectorizer = CountVectorizer(input = 'content', analyzer = 'char')

tf_vectorizer_bin = CountVectorizer(input = 'content', analyzer = 'char', binary = True)

tf = (tf_vectorizer.fit_transform(train_sents[1], train_sents[0])).toarray()

tfidf = (tfidf_vectorizer.fit_transform(train_sents[1], np.array(train_sents[0]))).toarray()

tf_bin = (tf_vectorizer_bin.fit_transform(train_sents[1], train_sents[0])).toarray()

tft = (tf_vectorizer.transform(test_sents[1])).toarray()

tfidft = (tfidf_vectorizer.transform(test_sents[1])).toarray()

tft_bin = (tf_vectorizer_bin.transform(test_sents[1])).toarray()

gnbtf = GaussianNB()
gnbtf.fit(tf, np.array(train_sents[0]))
pred = gnbtf.predict(tft)

gnbidf = GaussianNB()
gnbidf.fit(tfidf, np.array(train_sents[0]))
predidf = gnbidf.predict(tfidft)

gnbbin = GaussianNB()
gnbbin.fit(tf, np.array(train_sents[0]))
predbin = gnbbin.predict(tft_bin)


bnbtf = BernoulliNB()
bnbtf.fit(tf, np.array(train_sents[0]))
predbtf = bnbtf.predict(tft)

bnbidf = BernoulliNB()
bnbidf.fit(tfidf, np.array(train_sents[0]))
predbidf = bnbidf.predict(tfidft)

bnbbin = BernoulliNB()
bnbbin.fit(tf_bin, np.array(train_sents[0]))
predbbin = bnbbin.predict(tft_bin)

mnbtf = MultinomialNB()
mnbtf.fit(tf, np.array(train_sents[0]))
predmtf = mnbtf.predict(tft)

mnbidf = MultinomialNB()
mnbidf.fit(tfidf, np.array(train_sents[0]))
predmidf = mnbidf.predict(tfidft)

mnbbin = MultinomialNB()
mnbbin.fit(tf_bin, np.array(train_sents[0]))
predmbin = mnbbin.predict(tft_bin)

svctf = svm.SVC()
svctf.fit(tf, np.array(train_sents[0]))
predstf = svctf.predict(tft)

svcidf = svm.SVC()
svcidf.fit(tfidf, np.array(train_sents[0]))
predsidf = svcidf.predict(tfidft)

svcbin = svm.SVC()
svcbin.fit(tf_bin, np.array(train_sents[0]))
predsbin = svcbin.predict(tft_bin)

lintf = svm.LinearSVC()
lintf.fit(tf, np.array(train_sents[0]))
predltf = lintf.predict(tft)

linidf = svm.LinearSVC()
linidf.fit(tfidf, np.array(train_sents[0]))
predlidf = linidf.predict(tfidft)

linbin = svm.LinearSVC()
linbin.fit(tf_bin, np.array(train_sents[0]))
predlbin = linbin.predict(tft_bin)


fout.write("F-measures \n")

fout.write("Gaussian Naive Bayes with tf: \n" + "Micro:  " \
	+ str(f1_score(np.array(test_sents[0]), pred, average = 'micro')) + "\n" + "Macro:  "\
	+ str(f1_score(np.array(test_sents[0]), pred, average = 'macro')) + "\n")

fout.write("Gaussian Naive Bayes with tf-idf: \n" + "Micro:  " \
	+ str(f1_score(np.array(test_sents[0]), predidf, average = 'micro')) + "\n" + "Macro:  "\
	+ str(f1_score(np.array(test_sents[0]), predidf, average = 'macro')) + "\n")

fout.write("Gaussian Naive Bayes with bin: \n" + "Micro:  " \
	+ str(f1_score(np.array(test_sents[0]), predbin, average = 'micro')) + "\n" + "Macro:  "\
	+ str(f1_score(np.array(test_sents[0]), predbin, average = 'macro')) + "\n")

fout.write("Bernulli Naive Bayes with tf: \n" + "Micro:  " \
	+ str(f1_score(np.array(test_sents[0]), predbtf, average = 'micro')) + "\n" + "Macro:  "\
	+ str(f1_score(np.array(test_sents[0]), predbtf, average = 'macro')) + "\n")

fout.write("Bernulli Naive Bayes with tf-idf: \n" + "Micro:  " \
	+ str(f1_score(np.array(test_sents[0]), predbidf, average = 'micro')) + "\n" + "Macro:  "\
	+ str(f1_score(np.array(test_sents[0]), predbidf, average = 'macro')) + "\n")

fout.write("Bernulli Naive Bayes with bin: \n" + "Micro:  " \
	+ str(f1_score(np.array(test_sents[0]), predbbin, average = 'micro')) + "\n" + "Macro:  "\
	+ str(f1_score(np.array(test_sents[0]), predbbin, average = 'macro')) + "\n")

fout.write("Multinominal Naive Bayes with tf: \n" + "Micro:  " \
	+ str(f1_score(np.array(test_sents[0]), predmtf, average = 'micro')) + "\n" + "Macro:  "\
	+ str(f1_score(np.array(test_sents[0]), predmtf, average = 'macro')) + "\n")

fout.write("Multinominal Naive Bayes with tf-idf: \n" + "Micro:  " \
	+ str(f1_score(np.array(test_sents[0]), predmidf, average = 'micro')) + "\n" + "Macro:  "\
	+ str(f1_score(np.array(test_sents[0]), predmidf, average = 'macro')) + "\n")

fout.write("Multinominal Naive Bayes with bin: \n" + "Micro:  " \
	+ str(f1_score(np.array(test_sents[0]), predmbin, average = 'micro')) + "\n" + "Macro:  "\
	+ str(f1_score(np.array(test_sents[0]), predmbin, average = 'macro')) + "\n")

fout.write("Support Vector Classification with tf: \n" + "Micro:  " \
	+ str(f1_score(np.array(test_sents[0]), predstf, average = 'micro')) + "\n" + "Macro:  "\
	+ str(f1_score(np.array(test_sents[0]), predstf, average = 'macro')) + "\n")

fout.write("Support Vector Classification with tf-idf: \n" + "Micro:  " \
	+ str(f1_score(np.array(test_sents[0]), predsidf, average = 'micro')) + "\n" + "Macro:  "\
	+ str(f1_score(np.array(test_sents[0]), predsidf, average = 'macro')) + "\n")

fout.write("Support Vector Classification with bin: \n" + "Micro:  " \
	+ str(f1_score(np.array(test_sents[0]), predsbin, average = 'micro')) + "\n" + "Macro:  "\
	+ str(f1_score(np.array(test_sents[0]), predsbin, average = 'macro')) + "\n")

fout.write("Linear SVC with tf: \n" + "Micro:  " \
	+ str(f1_score(np.array(test_sents[0]), predltf, average = 'micro')) + "\n" + "Macro:  "\
	+ str(f1_score(np.array(test_sents[0]), predltf, average = 'macro')) + "\n")

fout.write("Linear SVC with tf-idf: \n" + "Micro:  " \
	+ str(f1_score(np.array(test_sents[0]), predlidf, average = 'micro')) + "\n" + "Macro:  "\
	+ str(f1_score(np.array(test_sents[0]), predlidf, average = 'macro')) + "\n")

fout.write("Linear SVC with bin: \n" + "Micro:  " \
	+ str(f1_score(np.array(test_sents[0]), predlbin, average = 'micro')) + "\n" + "Macro:  "\
	+ str(f1_score(np.array(test_sents[0]), predlbin, average = 'macro')) + "\n")
