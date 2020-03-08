package gopkg

import (
	"testing"
	"text/template"
)

func TestTemplate(t *testing.T) {
	t.Parallel()
	Template()
}

func BenchmarkTemplate(b *testing.B) {
	for i := 0; i < b.N; i++ {
		template.Must(template.New("go").Parse(pageTemplate))
	}
}
