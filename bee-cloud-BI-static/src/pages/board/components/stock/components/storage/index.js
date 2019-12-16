import { Component } from 'react';
import { Row, Col, Card, Form, DatePicker, Spin, Radio, message, Empty } from 'antd';
import styles from '../../index.less';
import { G2, Chart, Geom, Axis, Tooltip, Coord, Label, Legend, View, Guide, Shape, Facet, Util } from "bizcharts";
import DataSet from "@antv/data-set";
import { getStorage, getOutg } from '../../services/services'
import moment from 'moment';

const format = 'YYYY-MM-DD';

export default class Data extends Component {
  state = {
    dateTime: moment().format(format),
    goodsType: 1,
    oneData: [],
    oneLoading: false,
    twoData: [],
    twoLoading: false,
  }

  componentDidMount() {
    this.getBaseOne({ goodsType: 1 });
    this.getBaseTwo();
  }

  getBaseOne = (values) => {
    const { dateTime, goodsType } = this.state;
    let self = this;
    this.setState({
      oneLoading: true
    }, () => {
      let params = {
        goodsType,
        dateTime,
        ...values
      }
      getStorage(params).then(res => {
        if (res.code === 1) {
          let newArray = [];
          res.object.forEach(item => {
            if (item.productNumber >= 0) {
              newArray.push({
                ...item
              })
            }
          })
          self.setState({
            oneData: newArray,
          })
        } else {
        }
        self.setState({
          dateTime: params.dateTime,
          goodsType: params.goodsType,
          oneLoading: false
        })
      })
    })
  }

  getBaseTwo = (values) => {
    const { dateTime } = this.state;
    let self = this;
    this.setState({
      twoLoading: true
    }, () => {
      let params = {
        goodsType: 4,
        dateTime,
        ...values
      }
      getStorage(params).then(res => {
        if (res.code === 1) {
          let newArray = [];
          res.object.forEach(item => {
            if (item.productNumber >= 0) {
              newArray.push({
                ...item
              })
            }
          })
          self.setState({
            twoData: newArray,
          })
        } else {
        }
        self.setState({
          dateTime: params.dateTime,
          twoLoading: false
        })
      })
    })
  }

  onChangeOne = (goodsType) => {
    this.getBaseOne({ goodsType });
  }

  changeTime = (dateTime) => {
    this.getBaseOne({ dateTime });
    this.getBaseTwo({ dateTime });
  }

  render() {
    const { dateTime, goodsType, oneData, oneLoading, twoData, twoLoading } = this.state;

    return (
      <Card
        title="库存情况"
        extra={<div className={styles.buttonBox}>
          <DatePicker
            style={{ marginLeft: 24 }}
            allowClear={false}
            format={format}
            onChange={(date, dateString) => this.changeTime(dateString)}
            value={moment(dateTime)}
          />
        </div>}
      >
        <Row>
          <Col span={12}>
            <Spin spinning={oneLoading}>
              <div className={styles.col_header}>
                <span>原料库存情况</span>
                <Radio.Group onChange={(e) => this.onChangeOne(e.target.value)} value={goodsType}>
                  <Radio.Button value={1}>主料</Radio.Button>
                  <Radio.Button value={2}>辅料</Radio.Button>
                  <Radio.Button value={3}>其他</Radio.Button>
                </Radio.Group>
              </div>
              {
                oneData.length === 0 ?
                  <Empty /> :
                  <Chart height={400} data={oneData} padding={[40, 80, 80, 80]} forceFit>
                    <Axis name="productName" />
                    <Axis label={{ formatter: (text, item, index) => text + '吨' }} name="productNumber" />
                    <Tooltip
                      crosshairs={{ type: "cross" }}
                      itemTpl={'<li>' +
                        '<span style="background-color:{color};" class="g2-tooltip-marker"></span>' +
                        '<span class="li-name">库存重量：</span>' +
                        '<span>{value}</span>' +
                        '</li>'}
                    />
                    <Geom type="interval" position="productName*productNumber" />
                  </Chart>
              }
            </Spin>
          </Col>

          <Col span={12}>
            <Spin spinning={twoLoading}>
              <p className={styles.col_header}>
                <span>成品库存情况</span>
              </p>
              {
                twoData.length === 0 ?
                  <Empty /> :
                  <Chart height={400} data={twoData} padding={[40, 80, 80, 80]} forceFit>
                    <Axis name="productName" />
                    <Axis label={{ formatter: (text, item, index) => text + '吨' }} name="productNumber" />
                    <Tooltip
                      crosshairs={{ type: "cross" }}
                      itemTpl={'<li>' +
                        '<span style="background-color:{color};" class="g2-tooltip-marker"></span>' +
                        '<span class="li-name">库存重量：</span>' +
                        '<span>{value}</span>' +
                        '</li>'}
                    />
                    <Geom type="interval" position="productName*productNumber" />
                  </Chart>
              }
            </Spin>
          </Col>
        </Row>
      </Card>
    )
  }
}