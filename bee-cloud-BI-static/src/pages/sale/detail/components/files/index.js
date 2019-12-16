import React, { Component } from 'react'
import { Icon, Card, Empty } from 'antd';
import { connect } from 'dva';
import styles from './index.less';

@connect(({ sale: { orderInfo = {} } }) => ({ orderInfo }))
export default class Files extends Component {
  handleSkip = (url) => {
    if (url) {
      window.open(url);
    }
  }

  render() {
    const { orderInfo } = this.props;
    return (
      <div className={styles.container}>
        <Card title="合同附件">
          {
            orderInfo && orderInfo.contract && orderInfo.contract.files && orderInfo.contract.files.length ?
              orderInfo.contract.files.map((item, index) =>
                <div className={styles.item}>
                  <Icon type="paper-clip" />
                  <span onClick={this.handleSkip.bind(this, item.fileUrl)}>{item.fileName || '未知名字'}</span>
                </div>
              ) : <Empty />
          }
        </Card>
      </div>
    )
  }
}