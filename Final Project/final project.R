#install.packages("dplyr")
Donations <- read.csv("Donations.csv")
Donors <- read.csv("Donors.csv")
df_merged <- merge(Donations,Donors,by="Donor.ID")
# most amount of donations
most_amount <- max(df_merged$Donation.Amount)
most_amount_row <- df_merged[which(df_merged$Donation.Amount==most_amount),]
df<- colnames(df_merged)
df
donation_optional <- df_merged[which(df_merged$Donation.Included.Optional.Donation=="Yes"),]
donation_nonoptional <- df_merged[which(df_merged$Donation.Included.Optional.Donation=="No"),]
donation_optional_average <- mean(donation_optional$Donation.Amount)
donation_nonoptional_average <- mean(donation_nonoptional$Donation.Amount)
#table(df$Donor.Is.Teacher)
sample <- df_merged[sample(nrow(df),10000),]
teacher <- sample[which(sample$Donor.Is.Teacher== 'Yes'),]
nonteacher <- sample[which(sample$Donor.Is.Teacher=='No'),]
teacher_money_amount <- teacher$Donation.Amount
nonteacher_money_amount <- nonteacher$Donation.Amount
Resource <- read.csv("Resources.csv")
table(Resource$Resource.Unit.Price)
Resource_max_price <- max(Resource$Resource.Unit.Price, na.rm = TRUE)
Resource_name <- Resource[which(Resource$Resource.Unit.Price==Resource_max_price),]
Donated_amount_state <- aggregate(df_merged$Donation.Amount~df_merged$Donor.State,data=df_merged,sum)
write.csv(Donated_amount_state,"Donated_amount_state.csv")
#read.csv("Schools.csv")
Teachers <- read.csv("Teachers.csv")
#read.csv("Schools.csv")
res <- wilcox.test(teacher_money_amount, nonteacher_money_amount)
res

