## Kaggle competition "Facebook Recruiting IV: Human or Robot?"

### Description
In this competition, you'll be chasing down robots for an online auction site. Human bidders on the site are becoming increasingly frustrated with their inability to win auctions vs. their software-controlled counterparts. As a result, usage from the site's core customer base is plummeting.

In order to rebuild customer happiness, the site owners need to eliminate computer generated bidding from their auctions. Their attempt at building a model to identify these bids using behavioral data, including bid frequency over short periods of time, has proven insufficient. 

The goal of this competition is to identify online auction bids that are placed by "robots", helping the site owners easily flag these users for removal from their site to prevent unfair auction activity. 

There are two datasets in this competition. One is a bidder dataset that includes a list of bidder information, including their id, payment account, and address. The other is a bid dataset that includes 7.6 million bids on different auctions. The bids in this dataset are all made by mobile devices.

The online auction platform has a fixed increment of dollar amount for each bid, so it doesn't include an amount for each bid. You are welcome to learn the bidding behavior from the time of the bids, the auction, or the device. 

The data in this competition comes from an online platform, not from Facebook.

### Files
FacebookData.R does all the data preparation and contains some additional
exploratory steps that I found interesting. Not all created variables are 
included in the final model. I left this script in its "raw" form that was evolving
by gradually adding new ideas which means that it is not written in the most efficient way.

FacebookModelsCaret.R only contains the best final model, a Random Forest.
It achieves an AUC of 92.1% on the private test data.
