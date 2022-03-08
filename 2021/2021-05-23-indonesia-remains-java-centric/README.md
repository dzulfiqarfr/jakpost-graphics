# Overview

This project seeks to describe the relationship between economic development and population concentration in Java between 1971 and 2020. You can read the article on [this link](https://www.thejakartapost.com/news/2021/05/23/indonesia-remains-java-centric-despite-jokowis-infrastructure-campaign.html).


# Data

Data | Source |  
---- | ------ |  
Population (1971-2010) | [Statistics Indonesia (BPS)](https://www.bps.go.id/statictable/2009/02/20/1267/jumlah-penduduk-hasil-sensus-penduduk-sp-dan-survei-penduduk-antar-sensus-supas-menurut-provinsi-1971---2015.html) |  
Population (2020) | [BPS](https://www.bps.go.id/pressrelease/2021/01/21/1854/hasil-sensus-penduduk-2020.html) |  
Gross regional domestic product (GRDP) | [BPS](https://www.bps.go.id/indicator/171/533/1/-seri-2010-2-pdrb-atas-dasar-harga-konstan-menurut-pengeluaran-2010-100-.html) |  
Migration | [BPS](https://bps.go.id/publication/2020/12/02/725d484ca73434e95d4d4b9d/profil-migran-hasil-survei-sosial-ekonomi-nasional-2019.html) |  

The 2020 population and migration data are stored in PDFs, so I had to extract them using Tabula. I cleaned these data using Google Sheets.


# Changelog

The population and GDP distribution chart:  
- improved title, subtitle, caption and axis texts; and  
- used cividis as a single color palette to allow easier comparison across charts.

The GRDP per capita and share of incoming lifetime migrants chart:  
- shortened subtitle;  
- removed the population variable that was represented by circle size; and  
- reassigned the colors to "group" provinces in Java, Bali and Nusa Tenggara.

The share of net lifetime migrants map:  
- used smaller breaks.
