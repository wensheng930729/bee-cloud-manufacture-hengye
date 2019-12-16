
import { Component } from 'react';
import { Card } from 'antd';
import { getDeviceInspection } from '../../services/services';
import styles from '../../index.less';

const gridStyle = {
  width: '25%',
  textAlign: 'center',
  background: '#fff',
  height: 100,
  display: 'flex',
  flexDirection: 'column',
  justifyContent: 'center'
};


export default class Index extends Component {
  state = {
    data: {}
  }

  componentDidMount() {
    this.getDeviceInspection();
  }

  //获取设备数据
  getDeviceInspection = () => {
    getDeviceInspection().then(res => {
      if (res.code === 1) {
        this.setState({
          data: res.object
        })
      }
    })
  }

  render() {
    const { currentCount, waitCheck, checked, checkRate, normal, abnormal, runRate } = this.state.data;
    return (
      <Card title="设备数据" >
        <Card.Grid style={{ ...gridStyle, height: 200 }} >
          <p>今日巡检（台）</p>
          <p>{currentCount || '/'}</p>
        </Card.Grid>
        <Card.Grid style={gridStyle}>
          <p>今日待检（台）</p>
          <p>{waitCheck || '/'}</p>
        </Card.Grid>
        <Card.Grid style={gridStyle}>
          <p>检修设备（台）</p>
          <p>{checked || '/'}</p>
        </Card.Grid>
        <Card.Grid style={gridStyle}>
          <p>今日巡检率</p>
          <p>{checkRate || '/'}</p>
        </Card.Grid>
        <Card.Grid hoverable={false} style={gridStyle}>
          <p>正常运行（台）</p>
          <p>{normal || '/'}</p>
        </Card.Grid>
        <Card.Grid style={gridStyle}>
          <p>异常待修（台）</p>
          <p>{abnormal || '/'}</p>
        </Card.Grid>
        <Card.Grid style={gridStyle}>
          <p>设备运行率</p>
          <p>{runRate || '/'}</p>
        </Card.Grid>
      </Card>
    )
  }
}
