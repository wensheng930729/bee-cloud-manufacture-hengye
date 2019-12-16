import React, { PureComponent } from 'react';
import { connect } from 'dva';

class GridContent extends PureComponent {
  render() {
    const { contentWidth, children } = this.props;
    // let className = `${styles.main}`;
    // if (contentWidth === 'Fixed') {
      // className = `${styles.main} ${styles.wide}`;
    // }
    return <div >{children}</div>;
  }
}

export default connect(({ setting }) => ({
}))(GridContent);
