import { Component } from 'react';
import { Row, Col, Card, Spin, Radio, Empty } from 'antd';
import styles from '../../index.less';
import { G2, Chart, Geom, Axis, Tooltip, Coord, Label, Legend, View, Guide, Shape, Facet, Util } from "bizcharts";
import DataSet from "@antv/data-set";
import { getOnTheWay } from '../../services/services'

const { DataView } = DataSet;

export default class Data extends Component {
  state = {
    goodsType: 1,
    data: [],
    total: 0,
    loading: false,
  }

  componentDidMount() {
    this.getBaseData(1);
  }

  getBaseData = (goodsType) => {
    let self = this;
    this.setState({
      loading: true
    }, () => {
      getOnTheWay(goodsType).then(res => {
        if (res.code === 1) {
          let total = 0;
          res.object.forEach(item => {
            total += item.productNumber || 0;
          })
          self.setState({
            data: res.object,
            total
          })
        }
        self.setState({
          goodsType,
          loading: false
        })
      })
    })
  }

  onChangeType = (goodsType) => {
    this.getBaseData(goodsType);
  }

  legendData = (val) => {
    const { data, total } = this.state;
    let item = '';
    data.forEach((row, index) => {
      if (val === row.productName) {
        item = row;
      }
    })
    if (total === 0) {
      return `${val}：0% ${item.productNumber}吨`
    }
    return `${val}：${((item.productNumber / total) * 100).toFixed(2)}% ${item.productNumber}吨`
  }

  render() {
    const { goodsType, data, total, loading } = this.state;

    const dv = new DataView();
    dv.source(data).transform({
      type: "percent", // 转换类型
      field: "productNumber",
      dimension: "productName",
      as: "percent" // 结果存储
    });

    return (
      <Card title="在途情况">
        <Row>
          <Col span={24}>
            <Spin spinning={loading}>
              <div className={styles.col_header}>
                <span>在途库存</span>
                <Radio.Group onChange={(e) => this.onChangeType(e.target.value)} value={goodsType}>
                  <Radio.Button value={1}>主料</Radio.Button>
                  <Radio.Button value={2}>辅料</Radio.Button>
                  <Radio.Button value={3}>其他</Radio.Button>
                </Radio.Group>
              </div>
              {
                data.length === 0 ?
                  <Empty /> :
                  <Chart height={400} data={dv} scale={{ percent: { formatter: val => (val * 100).toFixed(2) + "%" } }} padding={[60, 240, 60, 0]} forceFit>
                    <Coord type="theta" radius={1} innerRadius={0.75} />
                    <Axis name="percent" />
                    <Legend
                      position="right-center"
                      offsetX={-30}
                      useHtml={true}
                      containerTpl={'<div class="g2-legend" style="position:absolute;top:20px;right:0px;width:auto;">'
                        + '<ul class="g2-legend-list" style="list-style-type:none;margin:0;padding:0;"></ul>'
                        + '</div>'}
                      itemTpl={'<li class="g2-legend-list-item item-{index} {checked}" data-color="{originColor}" data-value="{originValue}" style="cursor: pointer;font-size: 14px;">'
                        + '<i class="g2-legend-marker" style="width:10px;height:10px;border-radius:50%;display:inline-block;margin-right:10px;background-color: {color};"></i>'
                        + '<span class="g2-legend-text">{value}</span>'
                        + '</li>'}
                      itemMarginBottom={10}
                      itemFormatter={(val) => this.legendData(val)}
                      textStyle={{ fill: '#333333', fontSize: '14' }}
                    />
                    <Tooltip
                      showTitle={false}
                      itemTpl={'<li>' +
                        '<span style="background-color:{color};" class="g2-tooltip-marker"></span>' +
                        '<span class="li-name">{name}：</span>' +
                        '<span>{value}</span>' +
                        '</li>'}
                    />
                    <Guide>
                      <Guide.Html
                        position={["50%", "50%"]}
                        html={`<div style='color:#333333;font-size:18px;text-align: center;'>在途<br><span style='color:#333333;font-size:20px;font-weight: bold'>${total.toFixed(2)}吨</span></div>`}
                        alignX="middle"
                        alignY="middle"
                      />
                    </Guide>
                    <Geom type="intervalStack" position="percent" color="productName" style={{ lineWidth: 5, stroke: "#fff" }}
                      tooltip={["productName*percent", (productName, percent) => { percent = (percent * 100).toFixed(2) + "%"; return { name: productName, value: percent }; }]}
                    >
                    </Geom>
                  </Chart>
              }
            </Spin>
          </Col>
        </Row>
      </Card>
    )
  }
}