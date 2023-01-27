module ShareOnLinkedinFilter
  def linkedin_share_url(url)
    url = url_encode(url)
    "https://www.linkedin.com/sharing/share-offsite/?url=#{url}"
  end
end
# "https://www.linkedin.com/shareArticle?mini=true&url={articleUrl}&title={articleTitle}&summary={articleSummary}&source={articleSource}"
Liquid::Template.register_filter(ShareOnLinkedinFilter)
