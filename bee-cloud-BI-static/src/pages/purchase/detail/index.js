import React, { Component } from 'react'
import Wraper from './components/header'
import styles from './index.less'
import withRouter from "umi/withRouter"
import Components from './components'
import { connect } from "dva";

@withRouter
@connect(({ }) => ({
}))
export default class index extends Component {
  state = {
    tabList: [
      { tab: '详情', key: 'Detail' },
      { tab: '收货情况', key: 'Receive' },
      // { tab: '结算情况', key: 'Settlement' },
      { tab: '合同附件', key: 'Files' }
    ],
    activeKey: 'Detail'
  }

  componentDidMount() {
    this.contractBusinessId = this.props.location.query.contractBusinessId;
    const { dispatch } = this.props
    dispatch({
      type: "purchase/getBuyContractDetail",
      payload: this.contractBusinessId,
    })
  }
  
  //getTab
  onTabChange = (activeKey) => {
    this.setState({ activeKey })
  }

  render() {
    const { tabList, activeKey } = this.state;

    //子tab组件
    const ActiveTabComp = Components[activeKey];

    return (
      <div className={styles.container}>
        <Wraper tabList={tabList} activeKey={activeKey} onTabChange={this.onTabChange}>
          {ActiveTabComp ? <ActiveTabComp /> : null}
        </Wraper>
      </div>
    )
  }
}
